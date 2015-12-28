// Wrapper for the THREE.js library
var X = function(val) {
    var me =  {_this: val}
    me.map = function (t) { 
        console.log("me.map")
        return val.map = t
    }
    return me
}

// Should be called 
var surfaceCache = {};

function surface(args) {
  console.log(args,surfaceCache)
  var src = args.src || "";
  if (surfaceCache[src]) {
    var floorMaterial = surfaceCache[src];
  } else {
  	var floorTexture = new THREE.ImageUtils.loadTexture( src );
  	var floorMaterial = new THREE.MeshBasicMaterial( { 
        map: floorTexture,     /*  */
        side: THREE.DoubleSide /* both sides of this mesh are drawn */ 
    });
    surfaceCache[src] = floorMaterial;
  }
  floorMaterial.transparent = true;
  var size   = args.size   || new THREE.Vector2(100,100)
  var anchor = args.anchor || new THREE.Vector2(size.x * 0.5, size.y * 0.5)

  // We always create an anchor, and fasten our surface to this anchor.
  var center = new THREE.Object3D();
  var floorGeometry = new THREE.PlaneGeometry(size.x,size.y);
  var floor = new THREE.Mesh(floorGeometry, floorMaterial);

  console.log("anchor size",anchor,size)

  console.log((0.5 * size.x) - anchor.x ,- (0.5 * size.y) + anchor.y,0);
  floor.position.set((0.5 * size.x) - anchor.x ,-(0.5 * size.y) + anchor.y,0);
  center.add(floor);

  var sphere = new THREE.SphereGeometry( 5, 4, 4 );
  var material = new THREE.MeshBasicMaterial( { color: 0x000000} );
  mesh = new THREE.Mesh( sphere, material );
  center.add(mesh); 
 
  return center;
}
