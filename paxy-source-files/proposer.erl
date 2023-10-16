-module(proposer).
-export([start/6]).

-define(timeout, 800).
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
    %% Phase 1a: Proposers send "prepare" messages to all
    %% the acceptors. Done in function 'prepare'.
    prepare(Round, Acceptors),
    Quorum = (length(Acceptors) div 2) + 1,
    MaxVoted = order:null(),
    %% collect recopila promesas que los acceptors envían en la phase 1b.

    %% He cambiado length(Acceptors) por Quorum. Este parámetro es el número de promesas que necesitamos para aprender un valor, necesitamos la mayoría Quorum, no todos los Acceptors unánimemente.
    case collect(Quorum, Round, MaxVoted, Proposal) of
        {accepted, Value} ->
            io:format(
                "[Proposer ~w] Phase 2: round ~w proposal ~w (was ~w)~n",
                [Name, Round, Value, Proposal]
            ),
            % update gui
            PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Value},
            %% DUDA!! Value o Proposal? Value significa que aceptamos la propuesta de otro si es más votada (tiene sentido). Proposal significa encabezonarnos en nuestra propuesta.
            accept(Round, Value, Acceptors),
            %% MC: Le metemos Value, ya que los proposers no tienen que ser "egoistas"

            %% He cambiado también length(Acceptors) por Quorum. Igual que arriba, necesitamos solo los votos de la mayoría, no de todos
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
            %% Me llega promise con Value=na, eso quiere decir que el MaxVoted sera nuestro Round, ya que es el primero en hacer la request
            %% Poner MaxVoted para que el Round no sobre-escriba un MaxVoted que posiblemente sea mayor
            collect(N - 1, Round, MaxVoted, Proposal);
        {promise, Round, Voted, Value} ->
            %% Por que comparar Round con MaxVoted? La info nueva viene de Voted y Value. Es el voted de la response mayor que nuestro MaxVoted? Si es que si, quedarnos con lo que nos ha dicho la respuesta del Acceptor, su Voted es nuestro nuevo MaxVoted (y value el nuevo proposal)

            %% Si el ID que me prometen (Round) es mayor que el MaxVoted que he recolectado hasta ahora...
            case order:gr(Voted, MaxVoted) of
                true ->
                    %% Round es un nuevo máximo
                    %% Yep: The proposer then collects all promises and
                    %% also the voted value with the highest sequence number so far (collect())
                    %% Nuevo MaxVoted = Round y Proposal = Value. Round se queda igual????
                    collect(N - 1, Round, Voted, Value);
                false ->
                    %% Me han prometido mi ronda, no?
                    %% ...
                    collect(N - 1, Round, MaxVoted, Proposal)
            end;
        {promise, _, _, _} ->
            %% M-Comment: N-1. ya que hemos recibido promesa
            %% m-C: Este caso es imposible, ya que si acceptor devuelve promise, Round siempre sera nuestro Round (patern matching), y Voted/Value
            %% se les asignara el valor que nos da el acceptor, por lo tanto se queda en el segundo brack de promise, nunca llega al tercero (es esto cierto?)
            collect(N, Round, MaxVoted, Proposal);
        {sorry, {prepare, Round}} ->
            %% Sorry de Phase 1b. Acceptor ha prometido un Round
            %% mayor. Mantener estado (no se decrementa N porque
            %% no cuenta como promesa válido (es un sorry, lógico))
            %% Round, Proposal y MaxVoted deben quedar igual

            %%MC: I agree
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
        %% O han contestado todos o hay alguno que nunca devuelve respuesta, en este caso abortar y declarar como ronda fallida
        abort
    end.

vote(0, _) ->
    ok;
vote(N, Round) ->
    receive
        %% Este Pattern solo es válido cuando el mensaje que recibe tiene un Round que coincide con el de la llamada. Es decir, estamos recibiendo un voto a nuestro identificador de propuesta, así que decrementar la N y mantener el Round.
        {vote, Round} ->
            %%MC: Agree
            vote(N - 1, Round);
        %% Entonces aquí solo entra cuando recibimos un voto a una propuesta con ID mayor (porque si estoy aquí, me han hecho la promesa de no votar menores IDs, ¿no?)
        {vote, _} ->
            %% MC: El tema es que acceptor (tal como lo tenemos hecho) siempre devovlera nuestro Round, creo que este pattern matching es por posibles bugs, pero no tendria que llegar aqui nunca
            vote(N, Round);
        {sorry, {accept, Round}} ->
            %% Aquí recibe los sorrys tras hacer accept (Phase 2b). No cuentan para la mayoría (no decrementar N), y el Round se mantiene igualmente.
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
    if is_tuple(Name) -> %remote
        Name ! Message;
    true -> %local
        case whereis(Name) of
            undefined ->
                down;
            Pid ->
                Pid ! Message
        end
    end.