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

	atCenterX: 0.0,
	atCenterY: 0.0,

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
		screenHalfX: SCREEN_W/2;
		screenHalfY: SCREEN_H/2;
		//console.log("quad data: (" + this.x +", "+this.y+")");

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
//MARQUEE SELECT IS ALL F'd UP
		if (game.input.pointer1.isDown && game.input.pointer2.isDown){
			this.setQuads(game.input.pointer1.x, game.input.pointer1.y);

			this.oldX = game.input.pointer1.worldX;
			this.oldY = game.input.pointer1.worldY;
			this.nowX = game.input.pointer2.worldX;
			this.nowY = game.input.pointer2.worldY;
			/*
			this.oldX = game.input.pointer1.x;
			this.oldY = game.input.pointer1.y;
			this.nowX = game.input.pointer2.x;
			this.nowY = game.input.pointer2.y;
			*/  
			this.w = Math.abs(this.oldX - this.nowX);
			this.h = Math.abs(this.oldY - this.nowY);
	   		console.log("this.h, this.w : " + this.h + ", " + this.w);			
			/*
		    console.log("selectBox.bottom : " + selectBox.bottom);
		    console.log("selectBox.right : " + selectBox.right);
		    console.log("selectBox.top : " + selectBox.top);
		    console.log("selectBox.left : " + selectBox.left);

		    console.log("selectBox.x : " + selectBox.x);
		    console.log("selectBox.y : " + selectBox.y);
			*/

		    //selectBox.width = this.w;
		    //selectBox.height = this.h;
		    
		    var maxY = Math.max(this.oldY, this.nowY);
		    var minY = Math.min(this.oldY, this.nowY);
		    var maxX = Math.max(this.oldY, this.nowY);
		    var minX = Math.min(this.oldY, this.nowY);

			selectBox = game.add.graphics(minX,minY);
			selectBox.visible = false;
    	
    	
    		selectBox.lineStyle(2, 0x00ff00, 1);
    		selectBox.drawRect(minX, minY, this.h, this.w);

 			selectBox.fixedToCamera = true;
			
			console.log("this.w : " + this.w);
			
		}
		//else do this
		else {
			if(game.input.activePointer.isDown){
				this.setQuads();

				console.log("activePointer is down");
				this.oldX = game.input.activePointer.x;
				this.oldY = game.input.activePointer.y;
				
				//this.nowX = game.input.pointer2.x;
				//this.nowY = game.input.pointer2.y;

				//this.w = Math.abs(this.oldX - this.nowX);
				//this.h = Math.abs(this.oldY - this.nowY);

				this.centerY = this.oldY/SCREEN_H;
				this.centerX = this.oldX/SCREEN_W;

				if(this.centerX > 0.33 && this.centerX < 0.66){
			       	this.atCenterX = true;
			    }

				if(this.centerY > 0.33 && this.centerY < 0.66){
			       	this.atCenterY = true;
			    }

				//up
				 if(Math.round(this.centerY) === 1) {
				 	if(this.atCenterX){
				 		//do nothing
				 		//game.camera.y += 4;
				 	}else{
			       		game.camera.y += 4;
			    	}
			    	this.atCenterX = false;
			    }

			    //down
			    if(Math.round(this.centerY) === 0) {
			       	if(this.atCenterX){
				 		//do nothing
				 		//game.camera.y -= 4;
				 	}else{
			       		game.camera.y -= 4;
				    }
				    this.atCenterX = false;
			    }	

			    //left	
			    if(Math.round(this.centerX) === 0){
				 	if(this.atCenterY){
				 		//do nothing
				 		//game.camera.x -= 4;
				 	}else{
			        	game.camera.x -= 4;
			    	}
			    	this.atCenterY = false;
			    }

			    //right
			    if(Math.round(this.centerX) === 1){
				 	if(this.atCenterY){
				 		//do nothing
				 		//game.camera.x += 4;
				 	}else{
			    		game.camera.x += 4;
			    	}
			    	this.atCenterY = false;
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