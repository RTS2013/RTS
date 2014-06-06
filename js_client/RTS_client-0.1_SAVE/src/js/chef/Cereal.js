function Cereal(dv) {
   this.dv = dv;
   this.offset = 0;

   this.get8 = function() {
   		var val = this.dv.getInt8(this.offset);
   		this.offset = this.offset + 1;
   		return val;
   }

   this.getU8 = function() {
   		var val = this.dv.getUint8(this.offset);
   		this.offset = this.offset + 1;
   		return val;
   }

   this.get16 = function() {
   		var val = this.dv.getInt16(this.offset);
   		this.offset = this.offset + 2;
   		return val;
   }

   this.getU16 = function() {
   		var val = this.dv.getUint16(this.offset);
   		this.offset = this.offset + 2;
   		return val;
   }

   this.get32 = function() {
   		var val = this.dv.getInt32(this.offset);
   		this.offset = this.offset + 4;
   		return val;
   }

   this.getU32 = function() {
   		var val = this.dv.getUint32(this.offset);
   		this.offset = this.offset + 4;
   		return val;
   }

   this.getF32 = function() {
   		var val = this.dv.getFloat32(this.offset);
   		this.offset = this.offset + 4;
   		return val;
   }

   this.getF64 = function() {
   		var val = this.dv.getFloat64(this.offset);
   		this.offset = this.offset + 8;
   		return val;
   }

}

function uintToString(uintArray) {
    return decodeURIComponent(escape(atob(String.fromCharCode.apply(null, uintArray))));
}