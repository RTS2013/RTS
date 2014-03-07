//var surfaceMap = THREE.ImageUtils.loadTexture("./images/earth_surface_2048.jpg");
var material = new THREE.MeshBasicMaterial( {color: 0x00ff00} );
var geometry = new THREE.PlaneGeometry(400, 400, 100, 100);
var mesh = new THREE.Mesh(geometry, material);
mesh.geometry.dynamic = true;
scene.add(mesh);