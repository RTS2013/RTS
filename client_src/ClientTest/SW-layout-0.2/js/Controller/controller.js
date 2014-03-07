var kLeft 	= false,
	kRight 	= false,
	kUp 	= false,
	kDown	= false,
	kSpace	= false,
	kA 		= false,
	kS 		= false,
	kD 		= false,
	kF 		= false;


// call these listener functions in main.js
function addTouchHandler(){
	var dom = renderer.domElement;
	dom.addEvenListener( 'touchstart', onTouchStart, false);
	dom.addEvenListener( 'touchmove', onTouchMove, false);
}

function addKeyHandler(){
	var dom = renderer.domElement;
	dom.addEvenListener( 'keyup', onKeyUp, false);
	dom.addEvenListener( 'keypressed', onKeyPressed, true);
}

function addMouseHandler()
{
    var dom = renderer.domElement;
    dom.addEventListener( 'mouseup', onMouseUp, false);
}

							////////////////////////////
							// inner helper functions //
							////////////////////////////
/////////////////////////
// Touch
/////////////////////////

function onTouchStart(event){
	event.preventDefault();
}

function onTouchMove(event){
	event.preventDefault();
}

/////////////////////////
// Keyboard
/////////////////////////

function onKeyUp(event, key, pressed){
	event.preventDefault();
}

function onKeyPressed(event, key, pressed){
	event.preventDefault();
	//flip keyPessed boolean for given key
	switch(key){
		case KEY.LEFT: player.input 
	kLeft 	= false,
	kRight 	= false,
	kUp 	= false,
	kDown	= false,
	kSpace	= false,
	kA 		= false,
	kS 		= false,
	kD 		= false,
	kF 		= false;
	return event.keyCode;
}

/////////////////////////
// Mouse
/////////////////////////

function onMouseUp(event, key, pressed)
{
    event.preventDefault();
    //spinning cube animate boolean
    animating = !animating;

    //map deformation function

    if(onKeyPressed() == ){}
}

