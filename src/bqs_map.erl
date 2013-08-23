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
    ZoneWidth = ?ZONEWIDTH,
    ZoneHeight = ?ZONEHEIGHT,

    Grid = get_grid(Height, Width, get_json_value("collisions", Json)),
    
    Checkpoints = lists:map(fun get_checkpoint/1,
                            get_json_value("checkpoints", Json)),

    %% TODO filter the checkpoints that have s = 1 so that we get the correct
    %% startingAreas
    StartingAreas = Checkpoints,

    RoamingAreas = lists:map(fun get_mobarea/1,
                         get_json_value("roamingAreas", Json)),

    {_,StaticEntities} = get_json_value("staticEntities", Json),
                            
    PropList = [{"width", Width}, {"height", Height},
                {"zoneWidth", ZoneWidth}, {"zoneHeight", ZoneHeight},
                {"groupWidth", trunc(Width / ZoneWidth)},
                {"groupHeight", trunc(Height / ZoneHeight)},
                {"grid", Grid},
                {"startingAreas", StartingAreas},
                {"checkpoints", Checkpoints},
                {"mobAreas", RoamingAreas},
                {"staticEntities", StaticEntities}
               ],

    %spawn the enemies
    start_mob(RoamingAreas),
    
    Map = #map{checkpoints = Checkpoints,
               startingAreas = StartingAreas,
               json = Json,
               attributes = PropList
              },
    {ok, Map}.

handle_call(get_startingAreas, _From, Map) ->
    {reply, Map#map.startingAreas, Map};
handle_call({is_colliding, X, Y}, _From, #map{attributes = PL} = Map) ->
    Grid = proplists:get_value("grid", PL),
    {reply, do_is_colliding(X, Y, Grid), Map};
handle_call({is_out_of_bounds, X, Y}, _From, #map{attributes = PL} = Map) ->
    {reply, do_is_out_of_bounds(X, Y, PL), Map};
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

get_grid(Height, Width, Collisions) ->
    Grid = [{Y,X}||Y<-lists:seq(0,Height-1), X<-lists:seq(0,Width-1)],
    GridTiled = lists:zip(Grid, lists:seq(0,length(Grid)-1)),
    GridTree = gb_trees:from_orddict(GridTiled),
    CollisionTree = gb_trees:from_orddict(lists:zip(Collisions, Collisions)),
    gb_trees:map(set_array(CollisionTree), GridTree).

set_array(Collisions) ->
    fun(_Key, TileId) ->
            case gb_trees:lookup(TileId, Collisions) of
                none -> {TileId, 0};
                _ ->
                    {TileId, 1}
            end
    end.

do_is_colliding(X, Y, Grid) ->    
    {value, {_TileID, CollisionBit}} = gb_trees:lookup({X,Y}, Grid),
    CollisionBit == 1.

get_checkpoint(CP) ->
    [Id,X,Y,W,H] = [get_json_value(A, CP) || A <- ["id","x","y","w","h"]],
    #cp{id=Id,x=X,y=Y,w=W,h=H}.

get_mobarea(RoamingArea) ->
    [Id,X,Y,W,H,Type,Nb] = [get_json_value(A, RoamingArea) ||
                       A <- ["id","x","y","width","height","type","nb"]],
    #mobarea{id=Id,x=X,y=Y,w=W,h=H,type=Type,nb=Nb}.

do_is_out_of_bounds(X, Y, PL) ->
    Height = proplists:get_value("height", PL),
    Width = proplists:get_value("width", PL),
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
