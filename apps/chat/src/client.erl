-module(client).

-export([start_link/0]).
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([websocket_terminate/3]).

start_link() ->
  Dispatch =
    cowboy_router:compile(
      [
        {
          '_',
          [
            {"/", cowboy_static, {priv_file, websocket, "index.html"}},
            {
              "/websocket/:username/:password",
              [{username, nonempty}, {password, nonempty}],
              client,
              []
            },
            {"/static/[...]", cowboy_static, {priv_dir, websocket, "static"}}
          ]
        }
      ]
    ),
  {ok, Port} = application:get_env(websocket, port),
  {ok, Ip} = application:get_env(websocket, ip),
  {ok, _} = cowboy:start_clear(http, [{port, Port}, {ip, Ip}], #{env => #{dispatch => Dispatch}}).


init(Req, _Opts) ->
  Username = cowboy_req:binding(username, Req),
  Password = cowboy_req:binding(password, Req),
  {cowboy_websocket, Req, #{user => Username, password => Password}}.


websocket_init(#{user := Username, password := Password} = State) ->
  {Status, EnterMessage} = server:enter(Username, Password),
  case Status of
    ok -> server:add(Username, self());
    error -> logger:error("Error: ~p User: ~p", [EnterMessage, Username])
  end,
  {[{text, EnterMessage}], State}.


websocket_handle({text, Msg}, #{user := Username} = State) ->
  server:send_message(Username, Msg),
  {[], State};

websocket_handle(_Data, State) -> {[], State}.


websocket_info({send_message, _Ref, {User, Msg}}, State) ->
  {[{text, <<User/binary, ": ", Msg/binary>>}], State};

websocket_info({timeout, _Ref, _Msg}, State) -> {[], State};
websocket_info(_Info, State) -> {[], State}.

websocket_terminate(_Reason, _Req, #{user := Username} = _State) ->
  server:remove(Username),
  ok.
