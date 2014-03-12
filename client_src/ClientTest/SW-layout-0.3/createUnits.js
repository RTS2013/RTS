createUnits = function(){	
  	var boxCount = 5;

  	loader = new THREE.ColladaLoader();
  	loader.options.convertUpAxis = true;
  	loader.load('models/Everybody.dae', function (collada) {

    	var objectProto = collada.scene;

    	for (var i = 0; i < boxCount; i++) {

      		var object = objectProto.clone();

      		object.name = 'Unit #' + i;

      		object.rotation.z = Math.PI/180;

      		object.position.x = Math.random() * 800 - 400;
      		object.position.y = 0;
      		object.position.z = Math.random() * 800 - 400;

      		object.rotation.y = ( Math.random() * 360 ) * Math.PI / 180;

      		object.scale.x = 10;
      		object.scale.y = 10;
      		object.scale.z = 10;

      		scene.add(object);
   		}
  	});
		
}