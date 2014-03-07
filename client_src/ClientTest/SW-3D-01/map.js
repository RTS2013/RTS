// plane
var plane = new THREE.Mesh(new THREE.PlaneGeometry(300, 300), 
                           new THREE.MeshNormalMaterial());

function addPlane(){
    plane.overdraw = true;
    scene.add(plane);
}

