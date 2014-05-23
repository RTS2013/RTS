var IN = {
	//UI interaction modes
	NO_MODE: true,
	ADD_MODE: false,
	SUB_MODE: false,
	ACT_MODE: false,

	oldX: 10, oldY: 10,
	nowX: 40, nowY: 40,
	//w: Math.abs(this.oldX - this.nowX),
	//h: Math.abs(this.oldY - this.nowY)
	w: 0, h: 0,
	velX: 0.0, velY: 0.0,
	touchDrag: false,
	touchDown: false,
	touchUp: true,
	decayX: 0,
	decayY: 0,

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
		this.x = x;
		this.y = y;
		screenHalfX: game.SCREEN_W/2;
		screenHalfY: game.SCREEN_H/2;

		if(this.x < game.SCREEN_W/2 && this.y < game.SCREEN_H/2){
			inUpperLeft = true;
			inLowerLeft = false;
			inUpperRight = false;
			inLowerRight = false;
		}
		if(this.x < game.SCREEN_W/2 && this.y > game.SCREEN_H/2){
			inUpperLeft = false;
			inLowerLeft = true;
			inUpperRight = false;
			inLowerRight = false;
		}
		if(this.x > game.SCREEN_W/2 && this.y < game.SCREEN_H/2){
			inUpperLeft = false;
			inLowerLeft = false;
			inUpperRight = true;
			inLowerRight = false;
		
		}
		if(this.x > game.SCREEN_W/2 && this.y > game.SCREEN_H/2){
			inUpperLeft = false;
			inLowerLeft = false;
			inUpperRight = false;
			inLowerRight = true;
			
		}

		if(this.oldX < game.SCREEN_W/2){
			numX = -1; 
		}else{
			numX = 1;
		}
		//be careful of the reversed Y coordinates for screen movement
		if(this.oldY > game.SCREEN_H/2){
			numY = -1;
		}else{
			numY = 1;
		}
	},

	screenRolling: function(){
		game.camera.x += decayX;
		game.camera.y += decayY;
		decayX--;
		decayY--;
	},

	click: function(pointer) {
    	drawingLine = true;
    	console.log(""+selectBox);
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
	    else if (game.input.pointer1.isDown && game.input.pointer2.isDown){
	        //IN.nowX = game.input.pointer1.x;
	        //IN.nowY = game.input.pointer1.y;
	        //console.log("touch data X, Y: (" + IN.nowX + ", "+ IN.nowY +")");
	    	//console.log("Pointer 1 Down!!!");
	    }

	    if (game.input.pointer2.isDown){
	        //IN.nowX = game.input.pointer1.x;
	        //IN.nowY = game.input.pointer1.y;
	        //console.log("touch data X, Y: (" + IN.nowX + ", "+ IN.nowY +")");
	    	//console.log("Pointer 2 Down!!!");
	    }

	    if (game.input.pointer3.isDown){
	        //IN.nowX = game.input.pointer1.x;
	        //IN.nowY = game.input.pointer1.y;
	        //console.log("touch data X, Y: (" + IN.nowX + ", "+ IN.nowY +")");
	    	//console.log("Pointer 3 Down!!!");
	    }
		*/

		/*		
		//Screen drag functionality
		game.input.onDown.add(function(e){
		
		    if (Math.round(e.x/SCREEN_W) === 1) {
		       console.log("Right side onTap Registered");
		       game.camera.x += 4;
		    }
		    if (Math.round(e.y/SCREEN_H) === 1) {
		       console.log("Right side onTap Registered");
		       game.camera.y += 4;
		    }		
		    if (Math.round(e.x/SCREEN_W) === 0){
		        console.log("Left side onTap Registered");
		        game.camera.x -= 4;
		    }
		    if (Math.round(e.y/SCREEN_H) === 0){
		        console.log("Left side onTap Registered");
		    	game.camera.y -= 4;
		    }
		});
		*/
		//MARQUEE SELECT IS ALL F'd UP
		if (game.input.pointer1.isDown && (game.input.keyboard.isDown(cursors.SHIFT) || IN.ADD_MODE)){

			IN.nowX = game.input.pointer2.worldX;
			IN.nowY = game.input.pointer2.worldY;

			IN.w = Math.abs(IN.oldX - IN.nowX);
			IN.h = Math.abs(IN.oldY - IN.nowY);
			
		    var maxY = Math.max(IN.oldY, IN.nowY);
		    var minY = Math.min(IN.oldY, IN.nowY);
		    var maxX = Math.max(IN.oldY, IN.nowY);
		    var minX = Math.min(IN.oldY, IN.nowY);

			selectBox = game.add.graphics(minX,minY);
			selectBox.visible = false;
    	
    		selectBox.lineStyle(2, 0x00ff00, 1);
    		selectBox.drawRect(minX, minY, IN.h, IN.w);

 			selectBox.fixedToCamera = true;
			
			console.log("IN.w : " + IN.w);

			IN.oldX = IN.nowX;
			IN.oldY = IN.nowY;
			IN.ADD_MODE = false;

		}else{

			if(game.input.activePointer.isDown && HUD.notOverButtons(game.input.pointer1.x, game.input.pointer1.y)){
			
				if(IN.oldX != IN.nowX || IN.oldY != IN.nowY){
					
					IN.oldX = game.input.pointer1.x;
					IN.oldY = game.input.pointer1.y;
				}
				
				IN.nowX = game.input.pointer1.x;
				IN.nowY = game.input.pointer1.y;

				IN.setQuads(IN.oldX, IN.oldY);

				//upper left
				if(IN.inUpperLeft){
					//negative x, negative y
					//normalize pointer coordinates
					var normX = (IN.oldX - IN.screenMinX)/(SCREEN_W/2 - IN.screenMinX) -1; 
					var normY = (IN.oldY - IN.screenMinY)/(SCREEN_H/2 - IN.screenMinY) -1;

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

			if(IN.decayX > 0 || IN.decayY > 0){
				IN.screenRolling();
			}
		}
	}


};