function Chef() {
    this.ab = new ArrayBuffer(100);
    this.dv = new DataView(this.ab);
    this.offset = 0;

    this.resize = function (spaceNeeded) {
        if (this.ab.byteLength < this.offset + spaceNeeded) {
            var newAB = new ArrayBuffer((this.ab.byteLength + spaceNeeded) * 2);
            var newDV = new DataView(newAB);
            for (var i = 0; i < this.offset; i++) {
                newDV.setInt8(i,this.dv.getInt8(i));
            }
            this.dv = newDV;
            this.ab = newAB;
        }
    }
    
    this.trim = function (spaceNeeded) {
        var newAB = new ArrayBuffer(this.offset);
        var newDV = new DataView(newAB);
        for (var i = 0; i < this.offset; i++) {
            newDV.setInt8(i,this.dv.getInt8(i));
        }
        this.dv = newDV;
        this.ab = newAB;
    }

    this.putString = function(string) {
        var sv = new StringView(string);
        this.resize(sv.buffer.byteLength + 1);
        this.dv.setUint8(this.offset,sv.buffer.byteLength);
        this.offset = this.offset + 1;
        for (var i = 0; i < sv.buffer.byteLength; i++) {
            this.dv.setUint8(this.offset,sv.rawData[i]);
            this.offset++;
        }
    }

    this.put8 = function(v) {
        this.resize(1);
        this.setInt8(this.offset,v);
        this.offset = this.offset + 1;
    }

    this.putU8 = function(v) {
        this.resize(1);
        this.setUint8(this.offset,v);
        this.offset = this.offset + 1;
    }

    this.put16 = function(v) {
        this.resize(2);
        this.setInt16(this.offset,v);
        this.offset = this.offset + 2;
    }

    this.putU16 = function(v) {
        this.resize(2);
        this.setUint16(this.offset,v);
        this.offset = this.offset + 2;
    }

    this.putU32 = function(v) {
        this.resize(4);
        this.setUint32(this.offset,v);
        this.offset = this.offset + 4;
    }

    this.put32 = function(v) {
        this.resize(4);
        this.setInt32(this.offset,v);
        this.offset = this.offset + 4;
    }

    this.putF32 = function(v) {
        this.resize(4);
        this.setFloat32(this.offset,v);
        this.offset = this.offset + 4;
    }

    this.putF64 = function(v) {
        this.resize(8);
        this.setFloat64(this.offset,v);
        this.offset = this.offset + 8;
    }
}