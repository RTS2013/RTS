//Constructor
ScroungeApp = function(){
	Sim.App.call(this);
}

//Subclass Sim.app type
ScroungeApp.prototype = new Sim.App();

ScroungeApp.prototype.init = function(container, initX, initY, initZ){
	//Call superclass to setup scene and renderer
	Sim.App.prototype.init.call(this, container, initX, initY, initZ);

    var clickInfo = {
        x: 0,
        y: 0,
        userHasClicked: false

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
    function clicked(evt){
        clickInfo.userHasClicked = true;
        clickInfo.x = evt.clientX;
        clickInfo.y = evt.clientY;
    }


    function onWindowResize() {

        this.camera.aspect = SCREEN_WIDTH / SCREEN_HEIGHT;
        this.camera.updateProjectionMatrix();

        this.renderer.setSize( SCREEN_WIDTH, SCREEN_HEIGHT );
    }

    //this is where we initialize the different configs of the game
    //and ideally the Chef initialization should be able to tell us to load
    //different meshes that could be stored on the client but that's for later
    initGame();

}