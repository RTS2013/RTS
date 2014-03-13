/**
 * @author mrdoob / http://mrdoob.com/
 * @author alteredq / http://alteredqualia.com/
 * @author paulirish / http://paulirish.com/
 */

RTSControls = function (object, domElement) {

	this.object = object;
	this.target = new THREE.Vector3( 0, 0, 0 );

	this.domElement = ( domElement !== undefined ) ? domElement : document;

	this.autoSpeedFactor = 10;
	this.movementSpeed = 50;

	this.mouseX = 0;
	this.mouseY = 0;

	this.lat = 0;
	this.lon = 0;
	this.phi = 0;
	this.theta = 0;

	this.zoomIn = false;
	this.zoomOut = false;

	this.moveNear = false;
	this.moveFar = false;
	this.moveForward = false;
	this.moveBackward = false;
	this.moveLeft = false;
	this.moveRight = false;
	this.freeze = false;

	this.mouseDragOn = false;

	this.viewHalfX = window.innerWidth / 2;
	this.viewHalfY = window.innerWidth / 2;

	this.viewX = window.innerWidth;
	this.viewY = window.innerHeight;

	if ( this.domElement !== document ) {

		this.domElement.setAttribute( 'tabindex', -1 );

	}

	this.onMouseMove = function ( event ) {
		//move
		if(event.pageX < this.viewX * 0.01){
			this.moveLeft = true;
		} 
		if(event.pageX > this.viewX * 0.899){
			this.moveRight = true;
		} 
		if(event.pageY < this.viewY * 0.1){
			this.moveNear = true;
		} 
		if(event.pageY > this.viewY * 0.89){
			this.moveFar = true;
		}
		//stop moving
		if(event.pageX > this.viewX * 0.09){
			this.moveLeft = false;
		}
		if(event.pageX < this.viewX * 0.99){
			this.moveRight = false;
		} 
		if(event.pageY > this.viewY * 0.09){
			this.moveNear = false;
		}
		if(event.pageY < this.viewY * 0.89){
			this.moveFar = false;
		}


	};

	this.onKeyDown = function ( event ) {

		event.preventDefault();

		switch ( event.keyCode ) {

			case 38: /*up*/
			case 87: /*W*/ this.moveForward = true; 
					console.log(this.moveForward);
			break;
			
			case 37: /*left*/
			case 65: /*A*/ this.moveLeft = true; 
					console.log(this.moveLeft);
			break;

			case 40: /*down*/
			case 83: /*S*/ this.moveBackward = true; 
					console.log(this.moveBackward);
			break;

			case 39: /*right*/
			case 68: /*D*/ this.moveRight = true; 
					console.log(this.moveRight);
			break;

			case 82: /*R*/ this.moveUp = true; 
					console.log(this.moveUp);
			break;
			
			case 70: /*F*/ this.moveDown = true; 
					console.log(this.moveDown);
			break;

			case 81: /*Q*/ this.freeze = !this.freeze; break;

		}

	};

	this.onKeyUp = function ( event ) {

		switch( event.keyCode ) {

			case 38: /*up*/
			case 87: /*W*/ this.moveForward = false; break;

			case 37: /*left*/
			case 65: /*A*/ this.moveLeft = false; break;

			case 40: /*down*/
			case 83: /*S*/ this.moveBackward = false; break;

			case 39: /*right*/
			case 68: /*D*/ this.moveRight = false; break;

			case 82: /*R*/ this.moveUp = false; break;
			case 70: /*F*/ this.moveDown = false; break;

		}

	};

	this.update = function( delta ) {
		this.autoSpeedFactor = 0.0;
		var actualMoveSpeed = delta * this.movementSpeed;

		if ( this.moveForward ) this.object.translateZ( - ( actualMoveSpeed + this.autoSpeedFactor ) );
		if ( this.moveBackward ) this.object.translateZ( actualMoveSpeed );

		if ( this.moveLeft ) this.object.translateX( - actualMoveSpeed );
		if ( this.moveRight ) this.object.translateX( actualMoveSpeed );

		if ( this.moveUp ) this.object.translateY( actualMoveSpeed );
		if ( this.moveDown ) this.object.translateY( - actualMoveSpeed );

		if( this.moveFar ) this.object.position.z += actualMoveSpeed;
		if( this.moveNear ) this.object.position.z -= actualMoveSpeed;
		
	};


	this.domElement.addEventListener( 'contextmenu', function ( event ) { event.preventDefault(); }, false );

	this.domElement.addEventListener( 'mousemove', bind( this, this.onMouseMove ), false );
	this.domElement.addEventListener( 'keydown', bind( this, this.onKeyDown ), false );
	this.domElement.addEventListener( 'keyup', bind( this, this.onKeyUp ), false );

	function bind( scope, fn ) {

		return function () {

			fn.apply( scope, arguments );
		};
	};

	// we just do the following to hide the event from controls
	// and disable moving via mouse buttons
	var stopEvent = function (evt) {
	    evt.preventDefault();
	    evt.stopPropagation();
	};

	// The user has clicked; let's note this event
	// and the click's coordinates so that we can
	// react to it in the render loop
	this.clicked = function(evt){
		clickInfo.userHasClicked = true;
	 		clickInfo.x = evt.clientX;
			clickInfo.y = evt.clientY;
	}


	this.onWindowResize = function() {

		camera.aspect = SCREEN_WIDTH / SCREEN_HEIGHT;
		camera.updateProjectionMatrix();

		renderer.setSize( SCREEN_WIDTH, SCREEN_HEIGHT );

	}


};

/*
	this is where the touch handlers should go
*/