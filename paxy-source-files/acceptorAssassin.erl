-module(acceptorAssassin).
-export([start/2]).

-define(timeout, 200).

crash(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            pers:open(Name),
            {_, _, _, Pn} = pers:read(Name),
            Pn ! {updateAcc, "Voted: CRASHED", "Promised: CRASHED", {0,0,0}},
            pers:close(Name),
            unregister(Name),
            exit(Pid, "crash"),
            timer:sleep(600),
            register(Name, acceptor:start(Name, na))
    end.


start(N, AccRegs) when N>0 ->
    receive
        stop ->
            ok
    after ?timeout ->
        Victim = lists:nth(rand:uniform(length(AccRegs)), AccRegs),
        io:fwrite("CRASHING ~w~n", [Victim]),
        crash(Victim),
        start(N-1, AccRegs)
    end;
start(0, _) ->
    ok.