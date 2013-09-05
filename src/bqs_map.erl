%%%-------------------------------------------------------------------
%%% @author Gustav Simonsson  <gustav.simonsson@gmail.com>
%%% @doc
%%% Server-side map logic
%%% @end
%%% Created : 7 July 2012 by <gustav.simonsson@gmail.com>
%%%-------------------------------------------------------------------
-module(bqs_map).

-behaviour(gen_server).

-include("../include/bqs.hrl").
%% API
-export([start_link/1,
         get_startingAreas/0,
         is_colliding/2,
         is_out_of_bounds/2,
         tileid_to_pos/1,
         pos_to_tileid/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

%%%===================================================================
%%% API
%%%===================================================================
start_link(FilePath) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [FilePath], []).

%%%===================================================================
%%% Game API
%%%===================================================================
get_startingAreas() ->
    gen_server:call(?SERVER, get_startingAreas).

is_colliding(X, Y) ->
    gen_server:call(?SERVER, {is_colliding, X, Y}).

is_out_of_bounds(X, Y) ->
    gen_server:call(?SERVER, {is_out_of_bounds, X, Y}).

tileid_to_pos(TileId) ->
    gen_server:call(?SERVER, {tileid_to_pos, TileId}).

pos_to_tileid(X, Y) ->
    gen_server:call(?SERVER, {pos_to_tileid, {X, Y}}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([MapName]) ->
    File = code:priv_dir(bqs) ++ "/maps/" ++ MapName,
    {ok, FileBin} = file:read_file(File),
    Json = mochijson3:decode(FileBin),
    Height = get_json_value("height", Json),  
    Width = get_json_value("width", Json),
    Collisions = get_json_value("collisions", Json),

    Checkpoints = lists:map(fun get_checkpoint/1,
                            get_json_value("checkpoints", Json)),

    StartingAreas = [Cp || Cp <- Checkpoints, Cp#cp.s == true],

    RoamingAreas = lists:map(fun get_mobarea/1,
                         get_json_value("roamingAreas", Json)),

    {struct, Entities} = get_json_value("staticEntities", Json),
    StaticEntities = get_staticEntity(Entities, Width),
                            
    %spawn the enemies
    start_mob(RoamingAreas),
    start_mob(StaticEntities),

    Map = #map{checkpoints = Checkpoints,
               startingAreas = StartingAreas,
               height= Height,
               width = Width,
               json = Json,
               collisions = Collisions
              },
    {ok, Map}.

handle_call(get_startingAreas, _From, Map) ->
    {reply, Map#map.startingAreas, Map};
handle_call({is_colliding, X, Y}, _From, Map) ->
    {reply, do_is_colliding(X, Y, Map), Map};
handle_call({is_out_of_bounds, X, Y}, _From, Map) ->
    {reply, do_is_out_of_bounds(X, Y, Map), Map};
handle_call({tileid_to_pos, TileId}, _From, #map{width = Width} = Map) ->
    Res = tileid_to_pos(TileId, Width),
    {reply, Res, Map};
handle_call({pos_to_tileid, {X, Y}}, _From, #map{width = Width} = Map) ->
    Res = pos_to_tileid(X, Y, Width),
    {reply, Res, Map};
handle_call(Request, From, State) ->
    bqs_util:unexpected_call(?MODULE, Request, From, State),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    bqs_util:unexpected_cast(?MODULE, Msg, State),
    {noreply, State}.

handle_info(Info, State) ->
    bqs_util:unexpected_info(?MODULE, Info, State),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_json_value(Key, Json) ->
    mochijson3_helper:get_path_value([{1, binary:list_to_bin(Key)}], Json).

do_is_colliding(X, Y, Map) ->    
    TileId = pos_to_tileid(X, Y, Map#map.width),
    %% TODO, improve performance by putting collisions in an ets table and do a
    %% ets lookup
    lists:member(list_to_binary(integer_to_list(TileId)), Map#map.collisions).

get_checkpoint(CP) ->
    [Id, X, Y, W, H, S] = [get_json_value(A, CP) || A <- ["id", "x", "y", "w",
                                                       "h", "s"]],
    Start = bqs_util:integer_to_boolean(S),
    #cp{id=Id, x=X, y=Y, w=W, h=H, s=Start}.

get_mobarea(RoamingArea) ->
    [Id,X,Y,W,H,Type,Nb] = [get_json_value(A, RoamingArea) ||
                       A <- ["id","x","y","width","height","type","nb"]],
    #mobarea{id=Id,x=X,y=Y,w=W,h=H,type=Type,nb=Nb}.

get_staticEntity([], _) ->
    [];
get_staticEntity([{TileId, <<"rat">> = Type} | Entities], Width) ->
    {X, Y} = tileid_to_pos(list_to_integer(binary_to_list(TileId)), Width),
    Id = random:uniform(1000) + length(Entities),
    Nb = 1,
    [#mobarea{id = Id, type = Type, x = X, y = Y, w = X, h = Y, nb=Nb} |
     get_staticEntity(Entities, Width)];
get_staticEntity([_ | Entities], Width) ->
    get_staticEntity(Entities, Width).

do_is_out_of_bounds(X, Y, #map{height = Height, width = Width}) ->
    (X < 1) or (X >= Width) or (Y < 1) or (Y >= Height).    

%% TileId starts at 1 and maps to {0,0}
%% The TileId 0 is invalid because of this
tileid_to_pos(0, _) ->
    exit("Invalid TileId");
tileid_to_pos(TileId, Width) ->
    X = case TileId rem Width of
        0 ->
            Width - 1;
        Rem ->
            Rem -1
    end,
    Y = (TileId - 1) div Width,
    {X, Y}.

%% Calculates the TileId for pos X,Y
pos_to_tileid(X, Y, Width) ->
    (Y * Width) + X + 1.

start_mob([]) ->
    ok;
start_mob([#mobarea{nb = Nb} = Mob | Tail]) ->
    [add_mob(Mob)|| _ <- lists:seq(1, Nb)],
    start_mob(Tail).

add_mob(#mobarea{type = Type, x = X, y = Y, w = X, h = Y}) ->
    bqs_mob_sup:add_child(Type, X, Y);
add_mob(#mobarea{type = Type, x = X, y = Y, w = W, h = H}) ->
    StartX = X - 1 + random:uniform(W),
    StartY = Y - 1 + random:uniform(H),
    bqs_mob_sup:add_child(Type, StartX, StartY).
