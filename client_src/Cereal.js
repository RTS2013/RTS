function Cereal(dv)
{
   this.dv = dv;
   this.offset = 0;

   this.get8 = function() {
   		var val = getInt8(this.offset);
   		this.offset = this.offset + 8;
   		return val;
   }

   this.getU8 = function() {
   		var val = getUint8(this.offset);
   		this.offset = this.offset + 8;
   		return val;
   }

   this.get16 = function() {
   		var val = getInt16(this.offset);
   		this.offset = this.offset + 16;
   		return val;
   }

   this.getU16 = function() {
   		var val = getUint16(this.offset);
   		this.offset = this.offset + 16;
   		return val;
   }

   this.get32 = function() {
   		var val = getInt32(this.offset);
   		this.offset = this.offset + 32;
   		return val;
   }

   this.getU32 = function() {
   		var val = getUint32(this.offset);
   		this.offset = this.offset + 32;
   		return val;
   }

   this.getF32 = function() {
   		var val = getFloat32(this.offset);
   		this.offset = this.offset + 32;
   		return val;
   }

   this.getF64 = function() {
   		var val = getFloat64(this.offset);
   		this.offset = this.offset + 64;
   		return val;
   }
}

var client = new BinaryClient('ws://' + document.location.host);

client.on('stream', function(stream, meta){

	// collect stream data
	var parts = [];

	// data = ArrayBuffer
	stream.on('data', function(data){
		var dv = new DataView(data);
		// TODO
	});

	// when finished, set it as the background image
	stream.on('end', function(){
		// TODO
	});
});