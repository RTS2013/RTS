//Tiny class that belongs to Unit

// The rings
unitRing = function ( radius, nSegments, unit ) {
    var r = radius,
        s = nSegments,
        material = new THREE.LineBasicMaterial( { color: 0x00ff00 } ),
        geometry = new THREE.CircleGeometry( r, s );

    // Remove center vertex
    geometry.vertices.shift();

    //add it to the unit
    unit.ring = new THREE.Line(geometry, material);
    unit.ring.x = unit.x;
    unit.ring.y = unit.y;
    unit.ring.z = unit.z;
};