function Unit(uid,team,anim,type,vals,x,y,z,f,time) {
    this.unitID     = uid;
    this.unitTeam   = team;
    this.unitAnim   = anim;
    this.unitType   = team;
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
}

function Game(myTeamID,myName,mySecret,myStartVals) {
    this.myTeamID = myTeamID;
    this.myName   = myName;
    this.mySecret = mySecret;
    this.units    = new buckets.Dictionary(function (a){a.team + ":" + a.uid;});
    this.teamVals = new Array().concat(myStartVals);

    // Get game info from cereal
    this.getGameInfo = function(cereal) {
        var gramTime = cereal.getF64();
        var tag = cereal.getU8();
        switch(tag) {
        case 0:
            this.getUnitInfo(cereal,time);
            break;
        case 1:
            this.getTeamInfo(cereal,time);
            break;
        default:
            break;
        }
    }

   // Get units from cereal
    this.getUnitInfo = function(cereal,time) {
        var valCount = cereal.getU8();
        for (var i = 0; i < valCount; i++) {
            var uid = cereal.get32();
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
            var key = team + ":" + uid;
            if (this.units.containsKey(key)) {
                var u = this.units.get(key);
                if (u.updateTime < time) {
                    u.unitAnimation = anim;
                    u.unitType = type;
                    u.oldX = u.newX;
                    u.oldY = u.newY;
                    u.oldZ = u.newZ;
                    u.oldFacing = u.newFacing;
                    u.newX = x;
                    u.newY = y;
                    u.newZ = z;
                    u.newFacing = f;
                    u.updateTime = time;
                }
                
            }
            else {
                units.add(new Unit(uid,team,anim,type,vals,x,y,z,f));
            }
        }
    }

    // Get team info from cereal
    this.getTeamInfo = function(cereal,time) {
        var valCount = cereal.getU8();
        for (var i = 0; i < valCount; i++) {
            var key = cereal.getU8();
            var val = cereal.getU16();
            if (time > teamVals[key].updateTime) {
                this.teamVals[key] = {updateTime:time,value:val};
            }
        }
    }
}