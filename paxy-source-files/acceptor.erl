-module(acceptor).
-export([start/2]).

-define(delay, 500).

start(Name, PanelId) ->
    spawn(fun() -> init(Name, PanelId) end).

init(Name, PanelId) ->
    Promised = order:null(),
    Voted = order:null(),
    Value = na,
    acceptor(Name, Promised, Voted, Value, PanelId).

acceptor(Name, Promised, Voted, Value, PanelId) ->
    receive
        {prepare, Proposer, Round} ->
            case order:gr(Round, Promised) of
                true ->
                    %% Phase 1b: We have received a "prepare" message, and we can
                    %% promise this value because the message ID (Round) is greater
                    %% than the last promise made
                    T = rand:uniform(?delay),
                    timer:send_after(T, Proposer, {promise, Round, Voted, Value}),
                    io:format(
                        "[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                        [Name, Round, Voted, Value]
                    ),
                    % Update gui
                    Colour =
                        case Value of
                            na -> {0, 0, 0};
                            _ -> Value
                        end,
                    PanelId !
                        {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]),
                            "Promised: " ++ io_lib:format("~p", [Round]), Colour},
                    acceptor(Name, Round, Voted, Value, PanelId);
                false ->
                    Proposer ! {sorry, {prepare, Round}},
                    acceptor(Name, Promised, Voted, Value, PanelId)
            end;
        {accept, Proposer, Round, Proposal} ->
            %% Context: Acceptor promised to decline smaller Round accepts
            case order:goe(Round, Promised) of
                true ->
                    T = rand:uniform(?delay),
                    timer:send_after(T, Proposer, {vote, Round}),
                    %% Is the given Round bigger or equal than my Voted?
                    case order:goe(Round, Voted) of
                        true ->
                            io:format(
                                "[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                                [Name, Promised, Round, Proposal]
                            ),
                            % Update gui
                            PanelId !
                                {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]),
                                    "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
                            %% Proposer's Rounds is voted
                            acceptor(Name, Promised, Round, Proposal, PanelId);
                        false ->
                            %% My Voted wins and stays the same
                            acceptor(Name, Promised, Voted, Value, PanelId)
                    end;
                false ->
                    %% Context: In case of Round smaller than Promised, return sorry
                    Proposer ! {sorry, {accept, Round}},
                    acceptor(Name, Promised, Voted, Value, PanelId)
            end;
        stop ->
            io:format(
                "[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                [Name, Promised, Voted, Value]
            ),
            PanelId ! stop,
            ok
    end.
