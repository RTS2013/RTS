mousePicker = function(){
	if (this.clickInfo.userHasClicked){		  		

	    this.clickInfo.userHasClicked = false;

	    stats2.innerHTML = '';

	    // The following will translate the mouse coordinates into a number
	    // ranging from -1 to 1, where
	    //      x == -1 && y == -1 means top-left, and
	    //      x ==  1 && y ==  1 means bottom right
	    var x = ( clickInfo.x / SCREEN_WIDTH ) * 2 - 1;
	    var y = -( clickInfo.y / SCREEN_HEIGHT ) * 2 + 1;

	    // Now we set our direction vector to those initial values
	    directionVector.set(x, y, 1);

	    // Unproject the vector
	    projector.unprojectVector(directionVector, camera);

	    // Substract the vector representing the camera position
	    directionVector.sub(camera.position);

	    // Normalize the vector, to avoid large numbers from the
	    // projection and substraction
	    directionVector.normalize();

	    // Now our direction vector holds the right numbers!
	    raycaster.set(camera.position, directionVector);

	    // Ask the raycaster for intersects with all objects in the scene:
	    // (The second arguments means "recursive")
	    intersects = raycaster.intersectObjects(scene.children, true);

	    if (intersects.length) {
		    // intersections are, by default, ordered by distance,
		    // so we only care for the first one. The intersection
		    // object holds the intersection point, the face that's
		    // been "hit" by the ray, and the object to which that
		    // face belongs. We only care for the object itself.
		    var target = intersects[0].object;
		    stats2.innerHTML = 'Name: ' + target.name
		        + '<br>'
		        + 'ID: ' + target.id;

		    // let's move the marker to the hit point
		    marker.position.x = intersects[0].point.x;
		    marker.position.y = intersects[0].point.y;
		    marker.position.z = intersects[0].point.z;
    	}
	}
};