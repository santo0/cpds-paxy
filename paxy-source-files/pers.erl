-module(pers).
-export([open/1, read/1, store/5, close/1, delete/1]).

%% dets module provides term storage on file

open(Name) ->
    io:format(
    "PERSISTENCE MODULE [Acceptor ~w] OPENED PERSISTENCE FILE~n",
        [Name]
    ),
    dets:open_file(Name, []).

%% returns the object with the key 'perm' stored in the table 'Name'
read(Name) ->
    io:format(
    "PERSISTENCE MODULE [Acceptor ~w] READ PERSISTENCE FILE~n",
        [Name]
    ),
    case dets:lookup(Name, perm) of
        [{perm, Pr, Vt, Ac, Pn}] ->
            {Pr, Vt, Ac, Pn};
        [] ->
            {order:null(), order:null(), na, na}
    end.

%% inserts one object {Pr, Vt, Ac, Pn} into the table 'Name'
store(Name, Pr, Vt, Ac, Pn) ->
    io:format(
    "PERSISTENCE MODULE [Acceptor ~w] STORED ON PERSISTENCE FILE promised ~w voted ~w colour ~w panelid ~w~n",
        [Name, Pr, Vt, Ac, Pn]
    ),
    dets:insert(Name, {perm, Pr, Vt, Ac, Pn}).

close(Name) ->
    io:format(
    "PERSISTENCE MODULE [Acceptor ~w] CLOSE PERSISTENCE FILE~n",
        [Name]
    ),
    dets:close(Name).

delete(Name) ->
    io:format(
    "PERSISTENCE MODULE [Acceptor ~w] DELETE PERSISTENCE FILE~n",
        [Name]
    ),
    file:delete(Name).
