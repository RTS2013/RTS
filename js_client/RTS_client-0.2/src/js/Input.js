var IN = {
	//UI interaction modes
	NO_MODE: true,
	ADD_MODE: false,
	SUB_MODE: false,
	ACT_MODE: false,

	//regular touch (panning and marquee)
	oldX: 10, oldY: 10,
	nowX: 40, nowY: 40,

	//screen panning
	velX: 0.0, velY: 0.0,
	touchDrag: false,
	touchDown: false,
	touchUp: true,
	decayX: 0,
	decayY: 0,
	w: 0, h: 0,
	centerX: 0.0, centerY: 0.0,
   	
   	screenMinX: 0,
	screenMinY: 0,
	screenHalfX: 0,
	screenHalfY: 0,

	inUpperLeft: false,
	inUpperRight: false,
	inLowerLeft: false,
	inLowerRight: false,
	numX: 1,
	numY: 1,

	setQuads: function(x, y){
		//console.log("is IN.setQuads() even firing?");
		this.x = x;
		this.y = y;
		this.screenHalfX = SCREEN_W/2;
		this.screenHalfY = SCREEN_H/2;
		console.log("quad data screenHalfX, screenHalfY: (" + this.screenHalfX +", "+this.screenHalfY+")");

		if(this.x < this.screenHalfX && this.y < this.screenHalfY){
			this.inUpperLeft = true;
			this.inLowerLeft = false;
			this.inUpperRight = false;
			this.inLowerRight = false;
			console.log("quad data: inUpperLeft = " + this.inUpperLeft);
		}
		if(this.x < this.screenHalfX && this.y > this.screenHalfY){
			this.inUpperLeft = false;
			this.inLowerLeft = true;
			this.inUpperRight = false;
			this.inLowerRight = false;
			console.log("quad data: inLowerLeft = " + this.inLowerLeft);
		}
		if(this.x > this.screenHalfX && this.y < this.screenHalfY){
			this.inUpperLeft = false;
			this.inLowerLeft = false;
			this.inUpperRight = true;
			this.inLowerRight = false;
			console.log("quad data: inUpperRight = " + this.inUpperRight);
		}
		if(this.x > this.screenHalfX && this.y > this.screenHalfY){
			this.inUpperLeft = false;
			this.inLowerLeft = false;
			this.inUpperRight = false;
			this.inLowerRight = true;
			console.log("quad data: inLowerRight = " + this.inLowerRight);
		}

		if(this.oldX < SCREEN_W/2){
			numX = -1; 
		}else{
			numX = 1;
		}
		//be careful of the reversed Y coordinates for screen movement
		if(this.oldY > SCREEN_H/2){
			numY = -1;
		}else{
			numY = 1;
		}
	},

	screenRolling: function(){
		//game.camera.x += IN.decayX;
		//game.camera.y += IN.decayY;
		//IN.decayX--;
		//IN.decayY--;
	},

	drawBox: function(pointer) {
    	/*IN.*/ADD_MODE = true;
    	console.log("drawing selectBox: "+pointer.x+", "+pointer.y);
   	 	//selectLine.start.set(pointer.x, pointer.y);	
	},

	listen: function(){
		 //Mouse pointer
	    /*
	    if (game.input.mousePointer.isDown){
	        //IN.nowX = game.input.mousePointer.x;
	        //IN.nowY = game.input.mousePointer.y;
	        //console.log("touch data X, Y: (" + IN.nowX + ", "+ IN.nowY +")");
	    	//console.log("mouseDown!!!");
	    }
	    */
		//MARQUEE SELECT IS ALL F'd UP
		if (game.input.pointer2.isDown && 
			(game.input.keyboard.isDown(cursors.SHIFT) || IN.ADD_MODE) &&
			HUD.notOverButtons(game.input.pointer2.x, game.input.pointer2.y)){

			IN.oldX = IN.nowX;
			IN.oldY = IN.nowY;

			IN.nowX = game.input.activePointer.x;
			IN.nowY = game.input.activePointer.y;
			
			console.log("Marquee, oldX, oldY: " + IN.oldX + ", " + IN.oldY);
			console.log("Marquee, nowX, nowY: " + IN.nowX + ", " + IN.nowY);

		    var maxY = Math.max(IN.oldY, IN.nowY);
		    var minY = Math.min(IN.oldY, IN.nowY);
		    var maxX = Math.max(IN.oldX, IN.nowX);
		    var minX = Math.min(IN.oldX, IN.nowX);

			selectBox = game.add.graphics(minX,minY);
			console.log("minX, minY : " + minX + ", " + minY);
			selectBox.visible = false;
    	
    		selectBox.lineStyle(2, 0x00ff00, 1);
    		selectBox.drawRect(minX, minY, IN.h, IN.w);

 			selectBox.fixedToCamera = true;

			IN.oldX = IN.nowX;
			IN.oldY = IN.nowY;
			IN.ADD_MODE = false;

		}else{

			if( (game.input.pointer1.isDown || game.input.mousePointer.isDown) && 
			  HUD.notOverButtons(game.input.pointer1.x, game.input.pointer1.y)){
				//console.log("HUD.notOverButtons: " + game.input.pointer1.x+", " +game.input.pointer1.y);
				if(IN.oldX != IN.nowX || IN.oldY != IN.nowY){
					
					IN.oldX = game.input.pointer1.x;
					IN.oldY = game.input.pointer1.y;
				}
				
				IN.nowX = game.input.pointer1.x;
				IN.nowY = game.input.pointer1.y;	
				//console.log("IN.nowX, IN.nowY: " +IN.nowX+", "+IN.nowY);

				this.setQuads(IN.nowX, IN.nowY);
				console.log("IN: "+IN.oldX +", "+IN.oldY);

				//upper left
				if(IN.inUpperLeft){
					//negative x, negative y
					//normalize pointer coordinates
					var normX = (IN.oldX - IN.screenMinX)/(SCREEN_W/2 - IN.screenMinX) -1; 
					var normY = (IN.oldY - IN.screenMinY)/(SCREEN_H/2 - IN.screenMinY) -1;

					console.log("IN.inUpperLeft: "+IN.oldX +", "+IN.oldY);

					game.camera.x += normX * 20; //normX * scrollSpeed;
					game.camera.y += normY * 20;
					//game.camera.x += IN.velX;
					//game.camera.y += IN.velY;

					IN.decayX = IN.oldX - IN.nowX;
					IN.decayY = IN.oldY - IN.nowY;

					console.log("Registering upper left: " +normX +", "+normY);
				}
				//lower left
				if(IN.inLowerLeft){
					//negative x, positive y
					//normalize pointer coordinates
					var normX = (IN.oldX - IN.screenMinX)/(SCREEN_W/2 -IN.screenMinX) -1; 
					var normY = (IN.oldY - IN.screenHalfY)/(SCREEN_H - IN.screenHalfY);

					game.camera.x += normX * 20; //normX * scrollSpeed;
					game.camera.y += normY * 20;
					//game.camera.x += IN.velX;
					//game.camera.y += IN.velY;

					IN.decayX = IN.oldX - IN.nowX;
					IN.decayY = IN.oldY - IN.nowY;

					console.log("Registering lower left: " + normX +", "+normY);
				}
				//upper right
				if(IN.inUpperRight){
					//positive x, negative y
					//normalize pointer coordinates
					var normX = (IN.oldX - IN.screenHalfX)/(SCREEN_W - IN.screenHalfX);
					var normY = (IN.oldY - IN.screenMinY)/(SCREEN_H/2 - IN.screenMinY) -1; 

					game.camera.x += normX * 20; //normX * scrollSpeed;
					game.camera.y += normY * 20;
					//game.camera.x += IN.velX;
					//game.camera.y += IN.velY;

					IN.decayX = IN.oldX - IN.nowX;
					IN.decayY = IN.oldY - IN.nowY;
				
					console.log("Registering upper right: " + ((IN.oldX/SCREEN_W/2)) +", "+((IN.oldY/SCREEN_W/2) -1));
				}
				//lower right
				if(IN.inLowerRight){
					//positive x, positive y
					//normalize pointer coordinateX
					var normX = (IN.oldX - IN.screenHalfX)/(SCREEN_W - IN.screenHalfX);
					var normY = (IN.oldY - IN.screenHalfY)/(SCREEN_H - IN.screenHalfY);

					game.camera.x += normX * 20; //normX * scrollSpeed;
					game.camera.y += normY * 20;
					//game.camera.x += IN.velX;
					//game.camera.y += IN.velY;
								
					IN.decayX = IN.oldX - IN.nowX;
					IN.decayY = IN.oldY - IN.nowY;

					console.log("Registering lower right: " + normX +", "+normY);
				}
			}
			/*
			if(IN.decayX > 0 || IN.decayY > 0){
				IN.screenRolling();
			}
			*/
		}
	}
};