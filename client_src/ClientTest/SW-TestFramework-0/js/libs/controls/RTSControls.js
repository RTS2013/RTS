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

	//this.handleResize();

};

/*
	this is where the touch handlers should go
*/

/*
Sim.App.prototype.addDomHandlers = function()
{
	var that = this;
	window.addEventListener( 'resize', function(event) { that.onWindowResize(event); }, false );
}
/*
Sim.App.prototype.onDocumentMouseMove = function(event)
{
    event.preventDefault();
    
    if (this.clickedObject && this.clickedObject.handleMouseMove)
    {
	    var hitpoint = null, hitnormal = null;
	    var intersected = this.objectFromMouse(event.pageX, event.pageY);
	    if (intersected.object == this.clickedObject)
	    {
	    	hitpoint = intersected.point;
	    	hitnormal = intersected.normal;
	    }
		this.clickedObject.handleMouseMove(event.pageX, event.pageY, hitpoint, hitnormal);
    }
    else
    {
	    var handled = false;
	    
	    var oldObj = this.overObject;
	    var intersected = this.objectFromMouse(event.pageX, event.pageY);
	    this.overObject = intersected.object;
	
	    if (this.overObject != oldObj)
	    {
	        if (oldObj)
	        {
        		this.container.style.cursor = 'auto';
        		
        		if (oldObj.handleMouseOut)
        		{
        			oldObj.handleMouseOut(event.pageX, event.pageY);
        		}
	        }
	
	        if (this.overObject)
	        {
	        	if (this.overObject.overCursor)
	        	{
	        		this.container.style.cursor = this.overObject.overCursor;
	        	}
	        	
	        	if (this.overObject.handleMouseOver)
	        	{
	        		this.overObject.handleMouseOver(event.pageX, event.pageY);
	        	}
	        }
	        
	        handled = true;
	    }
	
	    if (!handled && this.handleMouseMove)
	    {
	    	this.handleMouseMove(event.pageX, event.pageY);
	    }
    }
}

Sim.App.prototype.onDocumentMouseDown = function(event)
{
    event.preventDefault();
        
    var handled = false;

    var intersected = this.objectFromMouse(event.pageX, event.pageY);
    if (intersected.object)
    {
    	if (intersected.object.handleMouseDown)
    	{
    		intersected.object.handleMouseDown(event.pageX, event.pageY, intersected.point, intersected.normal);
    		this.clickedObject = intersected.object;
    		handled = true;
    	}
    }
    
    if (!handled && this.handleMouseDown)
    {
    	this.handleMouseDown(event.pageX, event.pageY);
    }
}

Sim.App.prototype.onDocumentMouseUp = function(event)
{
    event.preventDefault();
    
    var handled = false;
    
    var intersected = this.objectFromMouse(event.pageX, event.pageY);
    if (intersected.object)
    {
    	if (intersected.object.handleMouseUp)
    	{
    		intersected.object.handleMouseUp(event.pageX, event.pageY, intersected.point, intersected.normal);
    		handled = true;
    	}
    }
    
    if (!handled && this.handleMouseUp)
    {
    	this.handleMouseUp(event.pageX, event.pageY);
    }
    
    this.clickedObject = null;
}

Sim.App.prototype.onDocumentMouseScroll = function(event, delta)
{
    event.preventDefault();

    if (this.handleMouseScroll)
    {
    	this.handleMouseScroll(delta);
    }
}

Sim.App.prototype.objectFromMouse = function(pagex, pagey)
{
	// Translate page coords to element coords
	var offset = $(this.renderer.domElement).offset();	
	var eltx = pagex - offset.left;
	var elty = pagey - offset.top;
	
	// Translate client coords into viewport x,y
    var vpx = ( eltx / this.container.offsetWidth ) * 2 - 1;
    var vpy = - ( elty / this.container.offsetHeight ) * 2 + 1;
    
    var vector = new THREE.Vector3( vpx, vpy, 0.5 );

    this.projector.unprojectVector( vector, this.camera );
	
    var raycaster = new THREE.Raycaster( this.camera.position, vector.sub( this.camera.position ).normalize() );

    raycaster.set( this.camera.position, vector );
	
    var intersects = raycaster.intersectObjects(this.scene.children, true);

    if ( intersects.length > 0 ) {    	
    	var target = intersects[0].object;	
    	statsNode.innerHTML = 'Name:' +target.name + '<br>' + 'ID' + target.id;
    }
}

Sim.App.prototype.findObjectFromIntersected = function(object, point, normal)
{
	if (object.data)
	{
		return { object: object.data, point: point, normal: normal };
	}
	else if (object.parent)
	{
		return this.findObjectFromIntersected(object.parent, point, normal);
	}
	else
	{
		return { object : null, point : null, normal : null };
	}
}


Sim.App.prototype.onKeyDown = function(event)
{
	// N.B.: Chrome doesn't deliver keyPress if we don't bubble... keep an eye on this
	event.preventDefault();

    if (this.handleKeyDown)
    {
    	this.handleKeyDown(event.keyCode, event.charCode);
    }
}

Sim.App.prototype.onKeyUp = function(event)
{
	// N.B.: Chrome doesn't deliver keyPress if we don't bubble... keep an eye on this
	event.preventDefault();

	if (this.handleKeyUp)
	{
		this.handleKeyUp(event.keyCode, event.charCode);
	}
}
	        
Sim.App.prototype.onKeyPress = function(event)
{
	// N.B.: Chrome doesn't deliver keyPress if we don't bubble... keep an eye on this
	event.preventDefault();

	if (this.handleKeyPress)
	{
		this.handleKeyPress(event.keyCode, event.charCode);
	}
}
*/