var KB = {
	//keyboard handler variable
	cursors: null,

	init: function(){
	    cursors = game.input.keyboard.createCursorKeys();
	    cursors.SHIFT = Phaser.Keyboard.SHIFT;
	},

	listen: function(){
		//keyboard handler for screen movement
		if (cursors.up.isDown){
	        game.camera.y -= 24;
	    }
	    else if (cursors.down.isDown){
	        game.camera.y += 24;
	    }
	    if (cursors.left.isDown){
	        game.camera.x -= 24;
	    }
	    else if (cursors.right.isDown){
	        game.camera.x += 24;
	    }

	    if(game.input.keyboard.isDown(cursors.SHIFT)){
	    	console.log("SHIFT is being pressed");

	    	////////////////start here///////////////////
	    	////////////////////////////////////////////
	    }
	}
};