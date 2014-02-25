/*
unitID
unitTeam
unitAnim
unitType
unitX
unitY
unitZ
unitFacing
[{key:int,val:int}]
*/

function Game() {
    this.teams = new Array();
}

function Team() {
    this.units = new buckets.Dictionary(function (a){"" + a.id;});
}

function Unit(id,team,anim,type,x,y,z,f,vals) {
    this.unitID     = id;
    this.unitTeam   = team;
    this.unitAnim   = anim;
    this.unitType   = team;
    this.unitX      = x;
    this.unitY      = y;
    this.unitZ      = z;
    this.unitFacing = f;
    this.unitValues = vals;
}

// Get a unit from your cereal
function getUnit(cereal) {
    var id = cereal.get32();
    var team = cereal.getU8();
    var anim = cereal.getU8();
    var type = cereal.getU16();
    var x = cereal.getU16();
    var y = cereal.getU16();
    var z = cereal.getU16();
    var f = cereal.getU8();
    var valCount = cereal.getU8();
    var vals = new Array();
    for (var i = 0; i < valCount; i++) {
        var key = cereal.getU8();
        var val = cereal.getU16();
        vals.push({k:key,v:val});
    }
    return new Unit(id,team,anim,type,x,y,z,f,vals);
}