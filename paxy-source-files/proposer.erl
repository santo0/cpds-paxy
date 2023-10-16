-module(proposer).
-export([start/6]).

-define(timeout, 2000).
-define(backoff, 10).

start(Name, Proposal, Acceptors, Sleep, PanelId, Main) ->
    spawn(fun() -> init(Name, Proposal, Acceptors, Sleep, PanelId, Main) end).

init(Name, Proposal, Acceptors, Sleep, PanelId, Main) ->
    timer:sleep(Sleep),
    Begin = erlang:monotonic_time(),
    Round = order:first(Name),
    {Decision, LastRound} = round(Name, ?backoff, Round, Proposal, Acceptors, PanelId),
    End = erlang:monotonic_time(),
    Elapsed = erlang:convert_time_unit(End - Begin, native, millisecond),
    io:format(
        "[Proposer ~w] DECIDED ~w in round ~w after ~w ms~n",
        [Name, Decision, LastRound, Elapsed]
    ),
    Main ! done,
    PanelId ! stop.

round(Name, Backoff, Round, Proposal, Acceptors, PanelId) ->
    io:format(
        "[Proposer ~w] Phase 1: round ~w proposal ~w~n",
        [Name, Round, Proposal]
    ),
    % Update gui
    PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Proposal},
    case ballot(Name, Round, Proposal, Acceptors, PanelId) of
        {ok, Value} ->
            {Value, Round};
        abort ->
            timer:sleep(rand:uniform(Backoff)),
            Next = order:inc(Round),
            round(Name, (2 * Backoff), Next, Proposal, Acceptors, PanelId)
    end.

ballot(Name, Round, Proposal, Acceptors, PanelId) ->
    %% Phase 1a: Proposers send "prepare" messages to all the acceptors.
    prepare(Round, Acceptors),
    Quorum = (length(Acceptors) div 2) + 1,
    MaxVoted = order:null(),
    %% collect procedure collects promises that the acceptors send in phase 1b

        case collect(Quorum, Round, MaxVoted, Proposal) of
        {accepted, Value} ->
            io:format(
                "[Proposer ~w] Phase 2: round ~w proposal ~w (was ~w)~n",
                [Name, Round, Value, Proposal]
            ),
            % update gui
            PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Value},
                        accept(Round, Value, Acceptors),
            
                        case vote(Quorum, Round) of
                ok ->
                    {ok, Value};
                abort ->
                    abort
            end;
        abort ->
            abort
    end.

collect(0, _, _, Proposal) ->
    {accepted, Proposal};
collect(N, Round, MaxVoted, Proposal) ->
    receive
        {promise, Round, _, na} ->
            %% I get promise with Value=na, this means that there is no Voted to compare to.
            collect(N - 1, Round, MaxVoted, Proposal);
        {promise, Round, Voted, Value} ->
                        case order:gr(Voted, MaxVoted) of
                true ->
                    %% The Voted received from acceptor is bigger than our MaxVoted, therefore
                    %% the Voted is now our MaxVoted and its Value our new Proposal
                    collect(N - 1, Round, Voted, Value);
                false ->
                    %% Our MaxVoted is for now the max Voted value I received, I keep our Proposal
                    collect(N - 1, Round, MaxVoted, Proposal)
            end;
        {promise, _, _, _} ->
                        collect(N, Round, MaxVoted, Proposal);
        {sorry, {prepare, Round}} ->
            %% Sorry from Phase 1b. Acceptor has promised a bigger Round. Mantain state ( N is not
            %%  decremented because it does not count as a valid promise (it's a sorry)
            %%  Round, Proposal and MaxVoted must remain the same
            io:format("[Proposer ] Phase 1: sorry prepare round ~w proposal ~w ~n", [
                Round, Proposal
            ]),
            collect(N, Round, MaxVoted, Proposal);
        {sorry, _} ->
            io:format("[Proposer ] Phase 1: sorry prepare round ~w proposal ~w ~n", [
                Round, Proposal
            ]),
            collect(N, Round, MaxVoted, Proposal)
    after ?timeout ->
                abort
    end.

vote(0, _) ->
    ok;
vote(N, Round) ->
    receive
        %% This Pattern is only valid when the message I receive has a Round that matches that of the call.
        %% I am receiving a vote for our proposal identifier, so I decrease the N and keep the Round.
        {vote, Round} ->
                        vote(N - 1, Round);
        %% It only enters here when I receive a vote for a proposal with a higher ID, not our proposal
        {vote, _} ->
                        vote(N, Round);
        {sorry, {accept, Round}} ->
            %% Here it receives the sorrys after accepting (Phase 2b). They do not count for the
            %% majority (do not decrease N), and the Round remains the same.
            vote(N, Round);
        {sorry, _} ->
            vote(N, Round)
    after ?timeout ->
        abort
    end.

prepare(Round, Acceptors) ->
    Fun = fun(Acceptor) ->
        send(Acceptor, {prepare, self(), Round})
    end,
    lists:foreach(Fun, Acceptors).

accept(Round, Proposal, Acceptors) ->
    Fun = fun(Acceptor) ->
        send(Acceptor, {accept, self(), Round, Proposal})
    end,
    lists:foreach(Fun, Acceptors).

send(Name, Message) ->
    Name ! Message.
