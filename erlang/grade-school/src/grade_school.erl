-module(grade_school).

-export([add/3, get/2, get/1, new/0]).

-spec add(nonempty_list(), integer(), map()) -> map().
add(Name, Grade,  School = #{grade_set := GradeSet}) ->
    GradeStudents = maps:get(Grade, School, []),
    NewGradeStudents = [Name | GradeStudents],
    NewGradeSet = sets:add_element(Grade, GradeSet), 
    School#{grade_set := NewGradeSet, Grade => NewGradeStudents}.

-spec get(nonempty_list(), map()) -> list().
get(Grade, School) -> 
    maps:get(Grade, School, []).

-spec get(map()) -> list().
get(School) -> 
    GradeSet = maps:get(grade_set, School),
    SortedGrades = lists:sort(sets:to_list(GradeSet)),
    lists:flatmap(
        fun (Grade) ->
            GradeStudents = maps:get(Grade, School),
            lists:sort(GradeStudents) 
        end, SortedGrades).
    
-spec new() -> map().
new() -> #{grade_set => sets:new()}.
