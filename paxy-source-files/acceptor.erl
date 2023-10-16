-module(acceptor).
-export([start/2]).

start(Name, PanelId) ->
    spawn(fun() -> init(Name, PanelId) end).

init(Name, PanelIdOrNa) ->
    pers:open(Name),
    case PanelIdOrNa of
        na -> {Promised, Voted, Value, ValidPanelId} = pers:read(Name),
            io:format(
                "LOADING [Acceptor ~w] READED FROM PERS: promised ~w voted ~w colour ~w~n",
                [Name, Promised, Voted, Value]
            ),
            Colour =
                case Value of
                    na -> {0, 0, 0};
                    _ -> Value
                end,
            ValidPanelId !
                {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]),
                    "Promised: " ++ io_lib:format("~p", [Promised]), Colour};

        ValidPanelId -> 
            io:format(
            "NOT LOADING [Acceptor ~w] Starting new acceptor - NOT FROM PERS~n",
                [Name]
            ),
            Promised = order:null(),
            Voted = order:null(),
            Value = na
    end,
    
    acceptor(Name, Promised, Voted, Value, ValidPanelId),
    io:format(
        "[Acceptor ~w] FINISHED~n",
        [Name]
    ),
    pers:close(Name),
    pers:delete(Name).

acceptor(Name, Promised, Voted, Value, PanelId) ->
    pers:store(Name, Promised, Voted, Value, PanelId),
    receive
        {prepare, Proposer, Round} ->
            case order:gr(Round, Promised) of
                true ->
                    %% Phase 1b: We have received a "prepare" message, and we can
                    %% promise this value because the message ID (Round) is greater
                    %% than the last promise made
                    Proposer ! {promise, Round, Voted, Value},
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
                    acceptor(Name, Promised, Voted, Value, PanelId)
            end;
        {accept, Proposer, Round, Proposal} ->
            %% Context: Acceptor promised to decline smaller Round accepts
            case order:goe(Round, Promised) of
                true ->
                    Proposer ! {vote, Round},
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
