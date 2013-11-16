-module(json_rec_tests).
-include_lib("eunit/include/eunit.hrl").

-record(simple, {
          one,
          two
          }).

-record(simplet2l, {
          two
          }).

-record(deep, {
          simple,
          second = []
         }).


-compile({parse_transform, exprecs}).
-export_records([simple,simplet2l,deep]).
-export([new/1]).

new(<<"simple">>) ->
    '#new-simple'();
new(<<"simplet2l">>) ->
    '#new-simplet2l'();
new(<<"deep">>) ->
    '#new-deep'();
new(_RecName) -> undefined.

simple_json_data() ->
    ["{\"one\":1,\"two\":2}",
     #simple{ one = 1, two = 2}].

simple_json_t2l_data() ->
    ["{\"two\":[1,2,3]}",
     #simplet2l{ two = {1,2,3}}].
unknown_json_data() ->
    ["{\"one\":1,\"two\":2}",
     [{<<"one">>, 1},{<<"two">>,2}]].

deep_json_data() ->
    Simple = "{\"simple\":{\"one\":1,\"two\":2}",
    Deep = Simple++"}",
    [Deep,
     #deep{ simple = #simple{ one = 1, two = 2}
            }].

deep_deep_json_data() ->
    Simple = "\"simple\":{\"one\":1,\"two\":2}",
    Deep = "{"++Simple++",\"second\":[{"++Simple ++ "},{" ++ Simple ++ "},{" ++ Simple ++"}]}",
    [Deep,
     #deep{ simple = #simple{ one = 1, two = 2},
            second  = [ #simple{ one = 1, two = 2},
                        #simple{ one = 1, two = 2},
                        #simple{ one = 1, two = 2}
                      ]
          }
     ].

to_rec_discriminator_test() ->
    DeepJson = "{\"one\": 1, \"two\": {\"two\": 2, \"__type\": \"simplet2l\"}, \"$type\": \"simple\"}",
    DeepRec = json_rec:to_rec(mochijson2:decode(DeepJson), ?MODULE, [<<"__type">>, <<"$type">>]),
    ?assertEqual(#simple{one=1, two=#simplet2l{two=2}}, DeepRec)
    .

to_json_discriminator_test() ->
    Rec = #simple{one=1, two=#simplet2l{two=2}},
    Json = lists:flatten(io_lib:format("~s", [mochijson2:encode(json_rec:to_json(Rec, ?MODULE, <<"__type">>))])),
    DiscriminatorJson = "{\"__type\":\"simple\",\"two\":{\"__type\":\"simplet2l\",\"two\":2},\"one\":1}",
    ?assertEqual(DiscriminatorJson, Json)
    .

discriminator_roundtrip_test() ->
    [_, Record] = deep_deep_json_data(),
    JSON = json_rec:to_json(Record, ?MODULE, <<"__type">>),
    RoundtripRecord = json_rec:to_rec(JSON, ?MODULE, <<"__type">>),
    ?assertEqual(Record, RoundtripRecord)
    .

simple_test() ->
    [Json, Rec] = simple_json_data(),
    NewRec = json_rec:to_rec(mochijson2:decode(Json),json_rec_tests,new(<<"simple">>)),
    ?assertEqual(Rec, NewRec).

deep_test() ->
    [Json, Rec] = deep_json_data(),
    NewRec = json_rec:to_rec(mochijson2:decode(Json),json_rec_tests,new(<<"deep">>)),
    ?assertEqual(Rec, NewRec).

deep_deep_test()  ->
    [Json, Rec] = deep_deep_json_data(),
    New = json_rec:to_rec(mochijson2:decode(Json),json_rec_tests,new(<<"deep">>)),
    ?assertEqual(Rec, New).

unknown_test() ->
    [Json, Rec] = unknown_json_data(),
    New = json_rec:to_rec(mochijson2:decode(Json),json_rec_tests,new(<<"unknown">>)),
    ?assertEqual(Rec, New).

to_json_simple_test() ->
    [_Json, Rec] = simple_json_data(),

    Conv = json_rec:to_json(Rec, json_rec_tests),
    Sjson= lists:flatten(mochijson2:encode(Conv)),

    New = json_rec:to_rec(mochijson2:decode(Sjson),json_rec_tests,new(<<"simple">>)),
    ?assertEqual(Rec,New).

to_json_deep_test() ->
    [_Json, Rec] = deep_json_data(),
    Conv = json_rec:to_json(Rec,json_rec_tests),
    Sjson = lists:flatten(mochijson2:encode(Conv)),


    New = json_rec:to_rec(mochijson2:decode(Sjson), json_rec_tests, new(<<"deep">>)),
    ?assertEqual(Rec,New).

to_json_deep_deep_test() ->
    [_Json, Rec] = deep_deep_json_data(),
    Conv = json_rec:to_json(Rec,json_rec_tests),
    Sjson = lists:flatten(mochijson2:encode(Conv)),
    New = json_rec:to_rec(mochijson2:decode(Sjson), json_rec_tests, new(<<"deep">>)),
    ?assertEqual(Rec,New).


