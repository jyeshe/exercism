-module(robot_simulator).

-export([robot_loop/0, advance/1, create/0, direction/1, left/1, place/3, position/1, right/1]).

-type property() :: direction | position.
-type direction() :: north | east | south | west.
-type side() :: left | right.
-type position() :: {X::Integer, Y::Integer}.
-type robot() :: pid().

-spec move(direction(), position()) -> position().
move(Direction, {X,Y}) ->
    case Direction of
        north ->
            {X, Y+1};
        south ->
            {X, Y-1};
        east ->
            {X+1, Y};
        west ->
            {X-1, Y}
    end.

-spec turn(direction(), side()) -> direction().
turn(Direction, right) ->
    case Direction of
        north -> east;
        east -> south;
        south -> west;
        west -> north
    end;

turn(Direction, left) ->
    case Direction of
        north -> west;
        west -> south;
        south -> east;
        east -> north
    end.

robot_loop() ->
    receive
        {place, Direction, Position} -> 
            put(direction, Direction),
            put(position, Position);
        {advance} ->
            Direction = get(direction),
            Position = get(position),
            put(position, move(Direction, Position));
        {turn, Side} ->
            Direction = get(direction),
            put(direction, turn(Direction, Side));
        {From, direction} ->
            From ! {direction, get(direction)};
        {From, position} ->
            From ! {position, get(position)}
    end,
    robot_loop().

-spec create() -> robot().
create() ->
    Pid = spawn(fun robot_simulator:robot_loop/0),
    Pid ! {place, north, {0,0}},
    Pid.

-spec get_property(robot(), property()) -> direction() | position().
get_property(Robot, Prop) ->
    Robot ! {self(), Prop},
    receive
        {Prop, Value} -> Value
    end.

-spec direction(robot()) -> direction().
direction(Robot) -> get_property(Robot, direction).
-spec position(robot()) -> position().
position(Robot) -> get_property(Robot, position).

-spec advance(robot()) -> ok.
advance(Robot) -> 
    Robot ! {advance}, 
    ok.

-spec left(robot()) -> ok.
left(Robot) -> 
    Robot ! {turn, left}, 
    ok.

-spec right(robot()) -> ok.
right(Robot) -> 
    Robot ! {turn, right}, 
    ok.

-spec place(robot(), direction(), position()) -> ok.
place(Robot, Direction, Position) -> 
    Robot ! {place, Direction, Position},
    ok.
