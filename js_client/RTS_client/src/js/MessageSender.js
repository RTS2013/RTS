// Send example
// Requires Chef

var sender = new Chef();

var sendMoveCommand = function(chef,unitIDArray,x,y,queue) {
		chef.putU8(1);
		chef.putU8(queue);
		chef.putU16(unitIDArray.length);
		for (i = 0; i < unitIDArray.length; i++) {
			chef.put32(unitIDArray[i]);
		}
		chef.trim();
	};