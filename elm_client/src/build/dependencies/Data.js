// Get game info from cereal
getGameInfo = function(cereal) {
    var time = cereal.getF64();
    var tag = cereal.getU8();
    switch(tag) {
    case 0:
        getUnitInfo(cereal,time);
        break;
    case 1:
        getTeamInfo(cereal,time);
        break;
    case 2:
        // TODO
        break;
    case 3:
        // TODO
        break;
    case 4:
        // TODO
        break;
    case 5:
        // TODO
        break;
    case 6:
        // TODO
        break;
    case 7:
        getPlaceInLife(cereal);
    default:
        break;
    }
}

getPlaceInLife = function(cereal) {
    var myTeamID = cereal.getU8();
    var x = cereal.getU16();
    var y = cereal.getU16();
    var myStart = {startX:x,startY:y};
    console.log("Your team " + myTeamID);
    return {teamID:myStart,startXY:myStart};
}

// Get team info from cereal
getTeamInfo = function(cereal,time) {
    var valCount = cereal.getU16();
    for (var i = 0; i < valCount; i++) {
        var key = cereal.getU8();
        var val = cereal.getU16();
        if (time > teamVals[key].updateTime) {
            teamVals[key] = {updateTime:time,value:val};
        }
    }
}

// Converts from degrees to radians.
Math.radians = function(degrees) {
  return degrees * Math.PI / 180;
};
 
// Converts from radians to degrees.
Math.degrees = function(radians) {
  return radians * 180 / Math.PI;
};

// Get units from cereal
getUnitInfo = function(cereal,time) {
    var unitArray = [];
    var valCount = cereal.getU16();

    for (var i = 0; i < valCount; i++) {
        var uid = cereal.get32();
        var team = cereal.getU8();
        var anim = cereal.getU8();
        var type = cereal.getU16();
        var x = cereal.getU16();
        var y = cereal.getU16();
        var z = cereal.getU16();
        var f = cereal.getU8();
        var valsCount = cereal.getU8();
        var vals = new Array();

        for (var j = 0; j < valsCount; j++) {
            var key = cereal.getU8();
            var val = cereal.getU16();
            vals.push({k:key,v:val});
        }

        var unit = 
            { unitID : uid
            , teamID : team
            , animID : anim
            , typeID : type
            , posX   : x/65536 * 1024
            , posY   : y/65536 * 1024
            , posZ   : z
            , facing : Math.radians(f*(360/256))
            , valueList : vals
            };

        unitArray.push(unit);
    }
    elm.ports.unitData.send({time:time,unitInfos:unitArray});
    elm.ports.unitData.send({time:time,unitInfos:[]});
}