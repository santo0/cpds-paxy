-module(paxy).
-export([start/1, stop/0, stop/1]).

-define(RED, {255, 0, 0}).
-define(GREEN, {0, 255, 0}).
-define(BLUE, {0, 0, 255}).
-define(PINK, {255, 0, 255}).
-define(ORANGE, {255, 165, 0}).
-define(PURPLE, {128, 0, 128}).
-define(BROWN, {165, 42, 42}).
-define(YELLOW, {255, 255, 0}).
-define(GRAY, {128, 128, 128}).
-define(CYAN, {0, 255, 255}).

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
    %AcceptorNames = ["Homer", "Marge", "Bart", "Lisa", "Maggie"],
    %AccRegister = [homer, marge, bart, lisa, maggie],
    AcceptorNames = ["Homer", "Marge", "Bart", "Lisa", "Maggie", "Apu", "Ned", "Lenny", "Carl", "Milhouse"],
    AccRegister = [homer, marge, bart, lisa, maggie, apu, ned, lenny, carl, milhouse],

    %ProposerNames = [{"Fry", ?RED}, {"Bender", ?GREEN}, {"Leela", ?BLUE}, {"Kyle", ?PINK}],
    %PropInfo = [{fry, ?RED}, {bender, ?GREEN}, {leela, ?BLUE}, {kyle, ?PINK}],
    ProposerNames = [
        {"Fry", ?RED}, {"Bender", ?GREEN}, {"Leela", ?BLUE}, {"Kyle", ?PINK},
        {"Amy", ?ORANGE}, {"Zoidberg", ?PURPLE}, {"Hermes", ?BROWN}, {"Nibbler", ?YELLOW},
        {"Scruffy", ?GRAY}, {"ProfessorFarnsworth", ?CYAN}
        ],

    PropInfo = [
        {fry, ?RED}, {bender, ?GREEN}, {leela, ?BLUE}, {kyle, ?PINK},
        {amy, ?ORANGE}, {zoidberg, ?PURPLE}, {hermes, ?BROWN}, {nibbler, ?YELLOW},
        {scruffy, ?GRAY}, {professorFarnsworth, ?CYAN}
        ],

    register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
    gui ! {reqState, self()},
    receive
        {reqState, State} ->
            {AccIds, PropIds} = State,
            start_acceptors(AccIds, AccRegister),
            spawn(fun() ->
                Begin = erlang:monotonic_time(),
                start_proposers(PropIds, PropInfo, AccRegister, Sleep, self()),
                wait_proposers(length(PropIds)),
                End = erlang:monotonic_time(),
                Elapsed = erlang:convert_time_unit(End - Begin, native, millisecond),
                io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed])
            end)
    end.

start_acceptors(AccIds, AccReg) ->
    case AccIds of
        [] ->
            ok;
        [AccId | Rest] ->
            [RegName | RegNameRest] = AccReg,
            register(RegName, acceptor:start(RegName, AccId)),
            start_acceptors(Rest, RegNameRest)
    end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep, Main) ->
    case PropIds of
        [] ->
            ok;
        [PropId | Rest] ->
            [{RegName, Colour} | RestInfo] = PropInfo,
            [FirstSleep | RestSleep] = Sleep,
            proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId, Main),
            start_proposers(Rest, RestInfo, Acceptors, RestSleep, Main)
    end.

wait_proposers(0) ->
    ok;
wait_proposers(N) ->
    receive
        done ->
            wait_proposers(N - 1)
    end.

stop() ->
    stop(homer),
    stop(marge),
    stop(bart),
    stop(lisa),
    stop(maggie),
    stop(gui).

stop(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop
    end.
