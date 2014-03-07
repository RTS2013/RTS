// Sim.js - A Simple Simulator for WebGL (based on Three.js)

Sim = {};

// Sim.Publisher - base class for event publishers
Sim.Publisher = function() {
    this.messageTypes = {};
}

Sim.Publisher.prototype.subscribe = function(message, subscriber, callback) {
    var subscribers = this.messageTypes[message];
    if (subscribers){
        if (this.findSubscriber(subscribers, subscriber) != -1){
            return;
        }
    }else{
        subscribers = [];
        this.messageTypes[message] = subscribers;
    }

    subscribers.push({ subscriber : subscriber, callback : callback });
}

Sim.Publisher.prototype.unsubscribe =  function(message, subscriber, callback) {
    if (subscriber)
    {
        var subscribers = this.messageTypes[message];

        if (subscribers)
        {
            var i = this.findSubscriber(subscribers, subscriber, callback);
            if (i != -1)
            {
                this.messageTypes[message].splice(i, 1);
            }
        }
    }
    else
    {
        delete this.messageTypes[message];
    }
}

Sim.Publisher.prototype.publish = function(message) {
    var subscribers = this.messageTypes[message];

    if (subscribers)
    {
        for (var i = 0; i < subscribers.length; i++)
        {
            var args = [];
            for (var j = 0; j < arguments.length - 1; j++)
            {
                args.push(arguments[j + 1]);
            }
            subscribers[i].callback.apply(subscribers[i].subscriber, args);
        }
    }
}

Sim.Publisher.prototype.findSubscriber = function (subscribers, subscriber) {
    for (var i = 0; i < subscribers.length; i++)
    {
        if (subscribers[i] == subscriber)
        {
            return i;
        }
    }
    
    return -1;
}

// Sim.App - application class (singleton)
Sim.App = function()
{
	Sim.Publisher.call(this);
	
	this.renderer = null;
	this.scene = null;
	this.camera = null;
	this.objects = [];
}

Sim.App.prototype = new Sim.Publisher;

Sim.App.prototype.init = function(param, initX, initY, initZ)
{
	
	if ( ! Detector.webgl ) Detector.addGetWebGLMessage();

	param = param || {};	
	
	var container = param.container;
	var canvas = param.canvas;
	
	var raycaster = new THREE.Raycaster();
    var projector = new THREE.Projector();
    var directionVector = new THREE.Vector3();    
    var clock = new THREE.Clock();
	var stats = new Stats(); 
	var statsNode = document.getElementById('stats');

    var loader, intersects, marker;

    // Create the Three.js renderer, add it to our div
    var renderer = new THREE.WebGLRenderer( { antialias: true, canvas: canvas } );
	renderer.setSize( SCREEN_WIDTH, SCREEN_HEIGHT );
    container.appendChild( renderer.domElement );

    // Create a new Three.js scene
    var scene = new THREE.Scene();
    scene.add( new THREE.AmbientLight( 0x505050 ) );
    scene.data = this;

    // Put in a camera at a good default location
    var camera = new THREE.PerspectiveCamera( 45, SCREEN_WIDTH / SCREEN_HEIGHT, 1, 10000 );
    camera.position.set(initX, initY, initZ);
    scene.add(camera);

    var controls = new RTSControls(camera, container);
    controls.movementSpeed = 100;
        
    // Create a root object to contain all other scene objects
    var root = new THREE.Object3D();
    scene.add(root);
    
    // Create a projector to handle picking
    var projector = new THREE.Projector();
    
    // Save away a few things
    this.container = container;
    this.renderer = renderer;
    this.scene = scene;
    this.camera = camera;
    this.projector = projector;
    this.root = root;
    this.controls = controls;
}

//Core run loop
Sim.App.prototype.run = function()
{
	var delta = clock.getDelta();

	var that = this;
	requestAnimationFrame(function() { that.run(); });	
	this.render(delta);

}

// Update method - called once per tick
Sim.App.prototype.render = function(delta)
{
	var i, len;
	len = this.objects.length;
	for (i = 0; i < len; i++)
	{
		this.objects[i].update(delta);
	}
	mousePicker();
	stats.update();
	controls.update(delta);
	this.renderer.render( this.scene, this.camera );
}

// Add/remove objects
Sim.App.prototype.addObject = function(obj)
{
	this.objects.push(obj);

	// If this is a renderable object, add it to the root scene
	if (obj.object3D)
	{
		this.root.add(obj.object3D);
	}
}

Sim.App.prototype.removeObject = function(obj)
{
	var index = this.objects.indexOf(obj);
	if (index != -1)
	{
		this.objects.splice(index, 1);
		// If this is a renderable object, remove it from the root scene
		if (obj.object3D)
		{
			this.root.remove(obj.object3D);
		}
	}
}

Sim.App.prototype.focus = function()
{
	if (this.renderer && this.renderer.domElement)
	{
		this.renderer.domElement.focus();
	}
}


// Sim.Object - base class for all objects in our simulation
Sim.Object = function()
{
	Sim.Publisher.call(this);
	
	this.object3D = null;
	this.children = [];
}

Sim.Object.prototype = new Sim.Publisher;

Sim.Object.prototype.init = function()
{
}

Sim.Object.prototype.update = function()
{
	this.updateChildren();
}

// setPosition - move the object to a new position
Sim.Object.prototype.setPosition = function(x, y, z)
{
	if (this.object3D)
	{
		this.object3D.position.set(x, y, z);
	}
}

//setScale - scale the object
Sim.Object.prototype.setScale = function(x, y, z)
{
	if (this.object3D)
	{
		this.object3D.scale.set(x, y, z);
	}
}

//setScale - scale the object
Sim.Object.prototype.setVisible = function(visible)
{
	function setVisible(obj, visible)
	{
		obj.visible = visible;
		var i, len = obj.children.length;
		for (i = 0; i < len; i++)
		{
			setVisible(obj.children[i], visible);
		}
	}
	
	if (this.object3D)
	{
		setVisible(this.object3D, visible);
	}
}

// updateChildren - update all child objects
Sim.Object.prototype.updateChildren = function()
{
	var i, len;
	len = this.children.length;
	for (i = 0; i < len; i++)
	{
		this.children[i].update();
	}
}

Sim.Object.prototype.setObject3D = function(object3D)
{
	object3D.data = this;
	this.object3D = object3D;
}

//Add/remove children
Sim.Object.prototype.addChild = function(child)
{
	this.children.push(child);
	
	// If this is a renderable object, add its object3D as a child of mine
	if (child.object3D)
	{
		this.object3D.add(child.object3D);
	}
}

Sim.Object.prototype.removeChild = function(child)
{
	var index = this.children.indexOf(child);
	if (index != -1)
	{
		this.children.splice(index, 1);
		// If this is a renderable object, remove its object3D as a child of mine
		if (child.object3D)
		{
			this.object3D.remove(child.object3D);
		}
	}
}

// Some utility methods
Sim.Object.prototype.getScene = function()
{
	var scene = null;
	if (this.object3D)
	{
		var obj = this.object3D;
		while (obj.parent)
		{
			obj = obj.parent;
		}
		
		scene = obj;
	}
	
	return scene;
}

Sim.Object.prototype.getApp = function()
{
	var scene = this.getScene();
	return scene ? scene.data : null;
}

// Some constants

/* key codes
37: left
38: up
39: right
40: down
*/
Sim.KeyCodes = {
    BACKSPACE: 8,
    TAB:       9,
    RETURN:   13,
    ESC:      27,
    SPACE:    32,
    PAGEUP:   33,
    PAGEDOWN: 34,
    END:      35,
    HOME:     36,
    LEFT:     37,
    UP:       38,
    RIGHT:    39,
    DOWN:     40,
    INSERT:   45,
    DELETE:   46,
    ZERO:     48, ONE: 49, TWO: 50, THREE: 51, FOUR: 52, FIVE: 53, SIX: 54, SEVEN: 55, EIGHT: 56, NINE: 57,
    A:        65, B: 66, C: 67, D: 68, E: 69, F: 70, G: 71, H: 72, I: 73, J: 74, K: 75, L: 76, M: 77, N: 78, O: 79, P: 80, Q: 81, R: 82, S: 83, T: 84, U: 85, V: 86, W: 87, X: 88, Y: 89, Z: 90,
    TILDA:    192
  };