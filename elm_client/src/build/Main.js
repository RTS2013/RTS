Elm.Main = Elm.Main || {};
Elm.Main.make = function (_elm) {
   "use strict";
   _elm.Main = _elm.Main || {};
   if (_elm.Main.values)
   return _elm.Main.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Main";
   var Basics = Elm.Basics.make(_elm);
   var Color = Elm.Color.make(_elm);
   var Data = Elm.Data.make(_elm);
   var Dict = Elm.Dict.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Collage = Elm.Graphics.Collage.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Element = Elm.Graphics.Element.make(_elm);
   var Grid = Elm.Grid.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Json = Elm.Native.Json.make(_elm);
   var Native = Native || {};
   Native.Ports = Elm.Native.Ports.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var Touch = Elm.Touch.make(_elm);
   var Transform2D = Elm.Transform2D.make(_elm);
   var Window = Elm.Window.make(_elm);
   var _op = {};
   var cthulhu = Graphics.Collage.toForm(A3(Graphics.Element.image,
   64,
   64,
   "build/resources/pure_evil.png"));
   var display = F2(function (g,
   _v0) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2":
            return function () {
                 var cameraY = g.camera.posY;
                 var cameraX = g.camera.posX;
                 var screenH = Basics.toFloat(_v0._1);
                 var screenW = Basics.toFloat(_v0._0);
                 var slice = A3(Basics.flip,
                 Grid.sliceBounds,
                 g.grid,
                 {ctor: "_Tuple4"
                 ,_0: Basics.ceiling(screenW / 64 + 1)
                 ,_1: Basics.ceiling(screenH / 64 + 1)
                 ,_2: Basics.floor((cameraX - screenW / 2) / 64)
                 ,_3: Basics.floor((cameraY - screenH / 2) / 64)});
                 var tiles = Graphics.Collage.group(A2(List.map,
                 function (_v4) {
                    return function () {
                       switch (_v4.ctor)
                       {case "_Tuple3":
                          return Graphics.Collage.move({ctor: "_Tuple2"
                                                       ,_0: Basics.toFloat(_v4._0) * 64 - cameraX
                                                       ,_1: Basics.toFloat(_v4._1) * 64 - cameraY})(Graphics.Collage.filled(A3(Color.rgb,
                            255,
                            255,
                            255))(Graphics.Collage.square(65)));}
                       _E.Case($moduleName,
                       "between lines 108 and 111");
                    }();
                 },
                 slice));
                 var units = Graphics.Collage.group(List.map(function (u) {
                    return Graphics.Collage.move({ctor: "_Tuple2"
                                                 ,_0: u.posX * 64 - cameraX
                                                 ,_1: u.posY * 64 - cameraY})(cthulhu);
                 })(List.filter(function (u) {
                    return _U.cmp(Math.pow(u.posX * 64 - cameraX,
                    2) + Math.pow(u.posY * 64 - cameraY,
                    2),
                    Math.pow(screenW / 2,
                    2) + Math.pow(screenH / 2,
                    2)) < 0;
                 })(A2(List.map,
                 Basics.snd,
                 Dict.values(g.units)))));
                 return A3(Graphics.Collage.collage,
                 _v0._0,
                 _v0._1,
                 _L.fromArray([tiles,units]));
              }();}
         _E.Case($moduleName,
         "between lines 96 and 118");
      }();
   });
   var updateUnits = F2(function (i,
   g) {
      return _U.replace([["units"
                         ,A3(List.foldl,
                         Data.updateUnitDict(i.unitData.time),
                         g.units,
                         i.unitData.unitInfos)]],
      g);
   });
   var updateGame = F2(function (i,
   g) {
      return function () {
         var _v9 = g.mode;
         switch (_v9.ctor)
         {case "Free":
            return i.middle ? function () {
                 var mode = Data.MoveCamera({_: {}
                                            ,cx: g.camera.posX
                                            ,cy: g.camera.posY
                                            ,mx: i.mouse.posX
                                            ,my: i.mouse.posY});
                 return A2(updateUnits,
                 i,
                 _U.replace([["input",i]
                            ,["mode",mode]],
                 g));
              }() : A2(updateUnits,
              i,
              _U.replace([["input",i]],g));
            case "MoveCamera":
            return i.middle ? function () {
                 var cam = g.camera;
                 var cam$ = _U.replace([["posX"
                                        ,_v9._0.cx - (g.input.mouse.posX - _v9._0.mx)]
                                       ,["posY"
                                        ,_v9._0.cy + (g.input.mouse.posY - _v9._0.my)]],
                 cam);
                 return A2(updateUnits,
                 i,
                 _U.replace([["input",i]
                            ,["camera",cam$]],
                 g));
              }() : A2(updateUnits,
              i,
              _U.replace([["input",i]
                         ,["mode",Data.Free]],
              g));}
         _E.Case($moduleName,
         "between lines 50 and 79");
      }();
   });
   var unitData = Native.Ports.portIn("unitData",
   Native.Ports.incomingSignal(function (v) {
      return typeof v === "object" && "time" in v && "unitInfos" in v ? {_: {}
                                                                        ,time: typeof v.time === "number" ? v.time : _E.raise("invalid input, expecting JSNumber but got " + v.time)
                                                                        ,unitInfos: _U.isJSArray(v.unitInfos) ? _L.fromArray(v.unitInfos.map(function (v) {
                                                                           return typeof v === "object" && "unitID" in v && "teamID" in v && "animID" in v && "typeID" in v && "posX" in v && "posY" in v && "posZ" in v && "facing" in v && "valueList" in v ? {_: {}
                                                                                                                                                                                                                                                                ,unitID: typeof v.unitID === "number" ? v.unitID : _E.raise("invalid input, expecting JSNumber but got " + v.unitID)
                                                                                                                                                                                                                                                                ,teamID: typeof v.teamID === "number" ? v.teamID : _E.raise("invalid input, expecting JSNumber but got " + v.teamID)
                                                                                                                                                                                                                                                                ,animID: typeof v.animID === "number" ? v.animID : _E.raise("invalid input, expecting JSNumber but got " + v.animID)
                                                                                                                                                                                                                                                                ,typeID: typeof v.typeID === "number" ? v.typeID : _E.raise("invalid input, expecting JSNumber but got " + v.typeID)
                                                                                                                                                                                                                                                                ,posX: typeof v.posX === "number" ? v.posX : _E.raise("invalid input, expecting JSNumber but got " + v.posX)
                                                                                                                                                                                                                                                                ,posY: typeof v.posY === "number" ? v.posY : _E.raise("invalid input, expecting JSNumber but got " + v.posY)
                                                                                                                                                                                                                                                                ,posZ: typeof v.posZ === "number" ? v.posZ : _E.raise("invalid input, expecting JSNumber but got " + v.posZ)
                                                                                                                                                                                                                                                                ,facing: typeof v.facing === "number" ? v.facing : _E.raise("invalid input, expecting JSNumber but got " + v.facing)
                                                                                                                                                                                                                                                                ,valueList: _U.isJSArray(v.valueList) ? _L.fromArray(v.valueList.map(function (v) {
                                                                                                                                                                                                                                                                   return typeof v === "object" && "k" in v && "v" in v ? {_: {}
                                                                                                                                                                                                                                                                                                                          ,k: typeof v.k === "number" ? v.k : _E.raise("invalid input, expecting JSNumber but got " + v.k)
                                                                                                                                                                                                                                                                                                                          ,v: typeof v.v === "number" ? v.v : _E.raise("invalid input, expecting JSNumber but got " + v.v)} : _E.raise("invalid input, expecting JSObject [\"k\",\"v\"] but got " + v);
                                                                                                                                                                                                                                                                })) : _E.raise("invalid input, expecting JSArray but got " + v.valueList)} : _E.raise("invalid input, expecting JSObject [\"unitID\",\"teamID\",\"animID\",\"typeID\",\"posX\",\"posY\",\"posZ\",\"facing\",\"valueList\"] but got " + v);
                                                                        })) : _E.raise("invalid input, expecting JSArray but got " + v.unitInfos)} : _E.raise("invalid input, expecting JSObject [\"time\",\"unitInfos\"] but got " + v);
   }));
   var middleDown = Native.Ports.portIn("middleDown",
   Native.Ports.incomingSignal(function (v) {
      return typeof v === "boolean" ? v : _E.raise("invalid input, expecting JSBoolean but got " + v);
   }));
   var rightDown = Native.Ports.portIn("rightDown",
   Native.Ports.incomingSignal(function (v) {
      return typeof v === "boolean" ? v : _E.raise("invalid input, expecting JSBoolean but got " + v);
   }));
   var leftDown = Native.Ports.portIn("leftDown",
   Native.Ports.incomingSignal(function (v) {
      return typeof v === "boolean" ? v : _E.raise("invalid input, expecting JSBoolean but got " + v);
   }));
   var mousePosition = Native.Ports.portIn("mousePosition",
   Native.Ports.incomingSignal(function (v) {
      return typeof v === "object" && "posX" in v && "posY" in v ? {_: {}
                                                                   ,posX: typeof v.posX === "number" ? v.posX : _E.raise("invalid input, expecting JSNumber but got " + v.posX)
                                                                   ,posY: typeof v.posY === "number" ? v.posY : _E.raise("invalid input, expecting JSNumber but got " + v.posY)} : _E.raise("invalid input, expecting JSObject [\"posX\",\"posY\"] but got " + v);
   }));
   var inputS = A2(Signal._op["~"],
   A2(Signal._op["~"],
   A2(Signal._op["~"],
   A2(Signal._op["~"],
   A2(Signal._op["~"],
   A2(Signal._op["<~"],
   F6(function (left,
   mid,
   right,
   pos,
   touches,
   unitData) {
      return {_: {}
             ,left: left
             ,middle: mid
             ,mouse: pos
             ,right: right
             ,touches: touches
             ,unitData: unitData};
   }),
   leftDown),
   middleDown),
   rightDown),
   mousePosition),
   Touch.touches),
   unitData);
   var gameS = A3(Signal.foldp,
   updateGame,
   Data.initGame,
   inputS);
   var main = A2(Signal._op["~"],
   A2(Signal._op["<~"],
   display,
   A2(Signal.sampleOn,
   Time.fps(10),
   gameS)),
   Window.dimensions);
   _elm.Main.values = {_op: _op
                      ,inputS: inputS
                      ,updateGame: updateGame
                      ,updateUnits: updateUnits
                      ,gameS: gameS
                      ,main: main
                      ,cthulhu: cthulhu
                      ,display: display};
   return _elm.Main.values;
};Elm.Data = Elm.Data || {};
Elm.Data.make = function (_elm) {
   "use strict";
   _elm.Data = _elm.Data || {};
   if (_elm.Data.values)
   return _elm.Data.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Data";
   var Basics = Elm.Basics.make(_elm);
   var Color = Elm.Color.make(_elm);
   var Dict = Elm.Dict.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Collage = Elm.Graphics.Collage.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Element = Elm.Graphics.Element.make(_elm);
   var Grid = Elm.Grid.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Json = Elm.Native.Json.make(_elm);
   var Native = Native || {};
   Native.Ports = Elm.Native.Ports.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var Touch = Elm.Touch.make(_elm);
   var Window = Elm.Window.make(_elm);
   var _op = {};
   var MoveCamera = function (a) {
      return {ctor: "MoveCamera"
             ,_0: a};
   };
   var Free = {ctor: "Free"};
   var initInput = {_: {}
                   ,left: false
                   ,middle: false
                   ,mouse: {_: {},posX: 0,posY: 0}
                   ,right: false
                   ,touches: _L.fromArray([])
                   ,unitData: {_: {}
                              ,time: 0
                              ,unitInfos: _L.fromArray([])}};
   var Input = F6(function (a,
   b,
   c,
   d,
   e,
   f) {
      return {_: {}
             ,left: a
             ,middle: b
             ,mouse: d
             ,right: c
             ,touches: e
             ,unitData: f};
   });
   var Camera = F4(function (a,
   b,
   c,
   d) {
      return {_: {}
             ,camAngle: d
             ,camSpeed: c
             ,posX: a
             ,posY: b};
   });
   var updateUnitDict = F3(function (time,
   ui,
   ud) {
      return function () {
         var $new = {_: {}
                    ,animID: ui.animID
                    ,facing: ui.facing
                    ,posX: ui.posX
                    ,posY: ui.posY
                    ,posZ: ui.posZ
                    ,time: time
                    ,typeID: ui.typeID
                    ,values: Dict.fromList(A2(List.map,
                    function (kv) {
                       return {ctor: "_Tuple2"
                              ,_0: kv.k
                              ,_1: kv.v};
                    },
                    ui.valueList))};
         var key = {ctor: "_Tuple2"
                   ,_0: ui.unitID
                   ,_1: ui.teamID};
         var maybePair = A2(Dict.get,
         key,
         ud);
         return function () {
            switch (maybePair.ctor)
            {case "Just":
               return A3(Dict.insert,
                 key,
                 {ctor: "_Tuple2"
                 ,_0: Basics.fst(maybePair._0)
                 ,_1: $new},
                 ud);
               case "Nothing":
               return A3(Dict.insert,
                 key,
                 {ctor: "_Tuple2"
                 ,_0: $new
                 ,_1: $new},
                 ud);}
            _E.Case($moduleName,
            "between lines 67 and 69");
         }();
      }();
   });
   var UnitInfo = F9(function (a,
   b,
   c,
   d,
   e,
   f,
   g,
   h,
   i) {
      return {_: {}
             ,animID: c
             ,facing: h
             ,posX: e
             ,posY: f
             ,posZ: g
             ,teamID: b
             ,typeID: d
             ,unitID: a
             ,valueList: i};
   });
   var Unit = F8(function (a,
   b,
   c,
   d,
   e,
   f,
   g,
   h) {
      return {_: {}
             ,animID: a
             ,facing: f
             ,posX: c
             ,posY: d
             ,posZ: e
             ,time: g
             ,typeID: b
             ,values: h};
   });
   var Game = F5(function (a,
   b,
   c,
   d,
   e) {
      return {_: {}
             ,camera: b
             ,grid: a
             ,input: c
             ,mode: d
             ,units: e};
   });
   var gridH = 128;
   var gridW = 128;
   var gridWH = {ctor: "_Tuple2"
                ,_0: gridW
                ,_1: gridH};
   var initCamera = {_: {}
                    ,camAngle: 0
                    ,camSpeed: 0
                    ,posX: gridW / 2 * 64
                    ,posY: gridH / 2 * 64};
   var initGame = {_: {}
                  ,camera: initCamera
                  ,grid: A2(Grid.make,gridWH,0)
                  ,input: initInput
                  ,mode: Free
                  ,units: Dict.empty};
   _elm.Data.values = {_op: _op
                      ,gridW: gridW
                      ,gridH: gridH
                      ,gridWH: gridWH
                      ,initGame: initGame
                      ,updateUnitDict: updateUnitDict
                      ,initCamera: initCamera
                      ,initInput: initInput
                      ,Free: Free
                      ,MoveCamera: MoveCamera
                      ,Game: Game
                      ,Unit: Unit
                      ,UnitInfo: UnitInfo
                      ,Camera: Camera
                      ,Input: Input};
   return _elm.Data.values;
};Elm.Grid = Elm.Grid || {};
Elm.Grid.make = function (_elm) {
   "use strict";
   _elm.Grid = _elm.Grid || {};
   if (_elm.Grid.values)
   return _elm.Grid.values;
   var _N = Elm.Native,
   _U = _N.Utils.make(_elm),
   _L = _N.List.make(_elm),
   _E = _N.Error.make(_elm),
   $moduleName = "Grid";
   var Array = Elm.Array.make(_elm);
   var Basics = Elm.Basics.make(_elm);
   var Color = Elm.Color.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Collage = Elm.Graphics.Collage.make(_elm);
   var Graphics = Graphics || {};
   Graphics.Element = Elm.Graphics.Element.make(_elm);
   var List = Elm.List.make(_elm);
   var Maybe = Elm.Maybe.make(_elm);
   var Native = Native || {};
   Native.Json = Elm.Native.Json.make(_elm);
   var Native = Native || {};
   Native.Ports = Elm.Native.Ports.make(_elm);
   var Signal = Elm.Signal.make(_elm);
   var String = Elm.String.make(_elm);
   var Text = Elm.Text.make(_elm);
   var Time = Elm.Time.make(_elm);
   var _op = {};
   var Grid = F3(function (a,b,c) {
      return {ctor: "Grid"
             ,_0: a
             ,_1: b
             ,_2: c};
   });
   var make = F2(function (_v0,a) {
      return function () {
         switch (_v0.ctor)
         {case "_Tuple2": return A2(Grid,
              _v0._0,
              _v0._1)(Array.fromList(A2(List.repeat,
              _v0._0 * _v0._1,
              a)));}
         _E.Case($moduleName,
         "on line 8, column 16 to 58");
      }();
   });
   var set = F3(function (_v4,
   a,
   _v5) {
      return function () {
         switch (_v5.ctor)
         {case "Grid":
            return function () {
                 switch (_v4.ctor)
                 {case "_Tuple2": return A2(Grid,
                      _v5._0,
                      _v5._1)(A3(Array.set,
                      _v5._0 * _v4._1 + _v4._0,
                      a,
                      _v5._2));}
                 _E.Case($moduleName,
                 "on line 11, column 30 to 65");
              }();}
         _E.Case($moduleName,
         "on line 11, column 30 to 65");
      }();
   });
   var get = F2(function (_v13,
   _v14) {
      return function () {
         switch (_v14.ctor)
         {case "Grid":
            return function () {
                 switch (_v13.ctor)
                 {case "_Tuple2":
                    return A2(Array.getOrFail,
                      _v14._0 * _v13._1 + _v13._0,
                      _v14._2);}
                 _E.Case($moduleName,
                 "on line 14, column 28 to 55");
              }();}
         _E.Case($moduleName,
         "on line 14, column 28 to 55");
      }();
   });
   var modify = F3(function (xy,
   f,
   g) {
      return function (a) {
         return A3(set,xy,a,g);
      }(f(A2(get,xy,g)));
   });
   var slice = F2(function (_v22,
   grid) {
      return function () {
         switch (_v22.ctor)
         {case "_Tuple4":
            return List.concat(A2(Basics.flip,
              List.map,
              _L.range(_v22._2 - 1,
              _v22._2 + _v22._0 - 1))(function (x) {
                 return A2(Basics.flip,
                 List.map,
                 _L.range(_v22._3 - 1,
                 _v22._3 + _v22._1 - 1))(function (y) {
                    return {ctor: "_Tuple3"
                           ,_0: x
                           ,_1: y
                           ,_2: A2(get,
                           {ctor: "_Tuple2",_0: x,_1: y},
                           grid)};
                 });
              }));}
         _E.Case($moduleName,
         "between lines 20 and 23");
      }();
   });
   var sliceBounds = F2(function (_v28,
   grid) {
      return function () {
         switch (_v28.ctor)
         {case "_Tuple4":
            return function () {
                 switch (grid.ctor)
                 {case "Grid":
                    return Maybe.justs(List.concat(A2(Basics.flip,
                      List.map,
                      _L.range(_v28._2,
                      _v28._2 + _v28._0))(function (x) {
                         return A2(Basics.flip,
                         List.map,
                         _L.range(_v28._3,
                         _v28._3 + _v28._1))(function (y) {
                            return _U.cmp(x,
                            grid._0) > -1 || (_U.cmp(x,
                            0) < 0 || (_U.cmp(y,
                            grid._1) > -1 || _U.cmp(y,
                            0) < 0)) ? Maybe.Nothing : Maybe.Just({ctor: "_Tuple3"
                                                                  ,_0: x
                                                                  ,_1: y
                                                                  ,_2: A2(get,
                                                                  {ctor: "_Tuple2"
                                                                  ,_0: x
                                                                  ,_1: y},
                                                                  grid)});
                         });
                      })));}
                 _E.Case($moduleName,
                 "between lines 26 and 34");
              }();}
         _E.Case($moduleName,
         "between lines 26 and 34");
      }();
   });
   _elm.Grid.values = {_op: _op
                      ,make: make
                      ,set: set
                      ,get: get
                      ,modify: modify
                      ,slice: slice
                      ,sliceBounds: sliceBounds
                      ,Grid: Grid};
   return _elm.Grid.values;
};