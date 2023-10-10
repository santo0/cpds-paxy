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
                    %% Proposer ! {promise, Round, Voted, Value},
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
                            "Promised: " ++ io_lib:format("~p", [Promised]), Colour},
                    acceptor(Name, Round, Voted, Value, PanelId);
                false ->
                    Proposer ! {sorry, {prepare, Round}},
                    %% T = rand:uniform(?delay),
                    %% timer:send_after(T, Proposer, {sorry, {prepare, Round}}),
                    acceptor(Name, Promised, Voted, Value, PanelId)
            end;
        {accept, Proposer, Round, Proposal} ->
            %% Context: Acceptor prometió rechazar prepare/accept con menor number
            case order:goe(Round, Promised) of
                true ->
                    %% !!!! He cambiado Proposal por Round. Lo que se tiene que devolver es un identificador, no un color
                    %% Proposer ! {vote, Round},
                    T = rand:uniform(?delay),
                    timer:send_after(T, Proposer, {vote, Round}),
                    %% M-C: Estoy de acuerdo
                    %% Context: Es el number que me han dado mayor o igual que el que he votado?
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
                            %% Round pasa a ser el voted
                            acceptor(Name, Promised, Round, Proposal, PanelId);
                        false ->
                            %% Voted queda invicto, no devuelve sorry o similar en este caso
                            acceptor(Name, Promised, Voted, Value, PanelId)
                    end;
                false ->
                    %% Context: Siempre en caso de tener numero menor que prometido, devolver sorry
                    Proposer ! {sorry, {accept, Promised}},
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
