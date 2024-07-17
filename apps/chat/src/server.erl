-module(server).

-behaviour(gen_server).

-export([add/2, remove/1, send_message/2, start_link/0, create_user/2, enter/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% вход пользователя
enter(User, Password) -> gen_server:call(?MODULE, {enter, {User, Password}}).

% добавление пользователя в список рассылки
add(User, Pid) -> gen_server:cast(?MODULE, {add, {User, Pid}}).

% удаление пользователя из списка рассылки
remove(User) -> gen_server:cast(?MODULE, {remove, User}).

% отправка сообщения пользователя списку рассылки
send_message(User, Message) -> gen_server:cast(?MODULE, {send_message, {User, Message}}).

% создание нового пользователя
create_user(User, Password) -> gen_server:cast(?MODULE, {create_user, {User, Password}}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) -> {ok, #{client_pids => [], client_data => []}}.

handle_call(
  {enter, {User, Password}},
  _From,
  #{client_pids := ClientPids, client_data := ClientData} = State
) ->
  IsExists = lists:member({User, Password}, ClientData),
  IsCOnnection = member_in_clients(User, ClientPids),
  {reply, enter_message(IsExists, IsCOnnection), State};

handle_call(stop, _From, State) -> {stop, normal, stopped, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.


handle_cast({add, {User, Pid}}, #{client_pids := ClientPids} = State) ->
  ClientPids1 = add_client({User, Pid}, ClientPids),
  {noreply, maps:update(client_pids, ClientPids1, State)};

handle_cast({remove, User}, #{client_pids := ClientPids} = State) ->
  ClientPids1 = remove_client(User, ClientPids),
  {noreply, maps:update(client_pids, ClientPids1, State)};

handle_cast({send_message, {User, Message}}, #{client_pids := ClientPids} = State) ->
  case member_in_clients(User, ClientPids) of
    true ->
      lists:foreach(
        fun ({_User, Pid}) -> Pid ! {send_message, self(), {User, Message}} end,
        remove_client(User, ClientPids)
      );

    false -> ok
  end,
  {noreply, State};

handle_cast({create_user, {User, Password}}, #{client_data := ClientData} = State) ->
  ClientData1 = [{list_to_binary(User), list_to_binary(Password)}] ++ ClientData,
  {noreply, maps:update(client_data, ClientData1, State)};

handle_cast(_Msg, State) -> {noreply, State}.


handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

% Добавляет пользователей в список рассылки без дублекатов
add_client({User, Pid}, [{User, _} | ClientPids]) -> [{User, Pid} | ClientPids];
add_client({User, Pid}, [Client | ClientPids]) -> [Client | add_client({User, Pid}, ClientPids)];
add_client({User, Pid}, []) -> [{User, Pid}].

% Удаляет пользователя из списка рассылки
remove_client(Client, ClientPids) -> remove_client(Client, ClientPids, []).

remove_client(User, [{User, _} | ClientPids], Acc) -> Acc ++ ClientPids;
remove_client(User, [Client | ClientPids], Acc) -> remove_client(User, ClientPids, [Client | Acc]);
remove_client(_, [], Acc) -> Acc.

% Проверяет находится ли пользователь в списке рассылки
member_in_clients(User, [{User, _} | _Clients]) -> true;
member_in_clients(User, [{_, _} | Clients]) -> member_in_clients(User, Clients);
member_in_clients(_User, []) -> false.

% Проверяет существует ли пользователь и находится ли он в списке рассылки
% {false, _} пользователя не существует
% {true, true} пользователь существует и уже подключен
% {true, false} пользователь сущетсвует, но не имеет подключений
enter_message(false, _) -> {error, <<"auth_error">>};
enter_message(true, true) -> {error, <<"always_connection">>};
enter_message(true, false) -> {ok, <<"Welcome!">>}.
