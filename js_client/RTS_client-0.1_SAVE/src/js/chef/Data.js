function Unit(uid,team,anim,type,vals,x,y,z,f,time) {
    this.unitID     = uid;
    this.unitTeam   = team;
    this.unitAnim   = anim;
    this.unitType   = type;
    this.unitValues = vals;
    this.oldX       = x;
    this.oldY       = y;
    this.oldZ       = z;
    this.oldFacing  = f;
    this.newX       = x;
    this.newY       = y;
    this.newZ       = z;
    this.newFacing  = f;
    this.updateTime = time;

    spawnEntity(this);
    //this needs to be uncommented
}

function Game(myName,mySecret) {
    this.myTeamID = null;
    this.myStart = null;
    this.myName   = myName;
    this.mySecret = mySecret;
    this.units    = new buckets.Dictionary();
    this.teamVals;

    // Get game info from cereal
    this.getGameInfo = function(cereal) {
        var time = cereal.getF64();
        var tag = cereal.getU8();
        console.log("logical step "+time);
        switch(tag) {
        case 0:
            this.getUnitInfo(cereal,time);
            break;
        case 1:
            this.getTeamInfo(cereal,time);
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
            this.getPlaceInLife(cereal);
        default:
            break;
        }
    }

    this.getPlaceInLife = function(cereal) {
        this.myTeamID = cereal.getU8();
        var x = cereal.getU16();
        var y = cereal.getU16();
        this.myStart = {startX:x,startY:y};
        console.log("Your team " + myTeamID);
    }

   // Get units from cereal
    this.getUnitInfo = function(cereal,time) {
        var valCount = cereal.getU16();
        console.log("Unit Count " + valCount);
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
            var dicKey = team + ":" + uid;
            if (this.units.containsKey(dicKey)) {
                var u = this.units.get(dicKey);
                if (u.updateTime < time) {
                    u.unitAnimation = anim;
                    u.unitType = type;
                    u.oldX = u.newX;
                    u.oldY = u.newY;
                    u.oldZ = u.newZ;
                    u.oldFacing = u.newFacing;
                    u.newX = x/65536 * 1024;
                    u.newY = y/65536 * 1024;
                    u.newZ = z;
                    u.newFacing = toRadians(f*(360/256));
                    u.updateTime = time;
                }
            }
            else {
			var newUnit = new Unit(uid,team,anim,type,vals,x/65536 * 1024,
                                                                y/65536 * 1024,
                                                                z,
                                                                Math.radians(f*(360/256)));
            this.units.set(dicKey,newUnit);
            }
        }
    }

    // Get team info from cereal
    this.getTeamInfo = function(cereal,time) {
        var valCount = cereal.getU16();
        for (var i = 0; i < valCount; i++) {
            var key = cereal.getU8();
            var val = cereal.getU16();
            if (time > teamVals[key].updateTime) {
                this.teamVals[key] = {updateTime:time,value:val};
            }
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