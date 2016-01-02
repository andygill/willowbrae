// File:js/Willowbrae.js

/**
 * @author andygill
 */

var WILLOWBRAE = { REVISION: '0' };
var X = { REVISION: '0' };

// Taken from Three.js. Allows the library to be included in various build systems.
if ( typeof define === 'function' && define.amd ) {

		define( 'three', X );

} else if ( 'undefined' !== typeof exports && 'undefined' !== typeof module ) {

		module.exports = X;

}

// Wrapper for the THREE.js library that provides support for building rooms.
// Template originally taken from Three.js "tutorials by example" by Lee Stemkoski,
// though it has been though many changes now.


// Not sure this should all be done in the constructor

// opts.init = function(Theater t) { initial objects }


WILLOWBRAE.Theater = function ( opts ) {
  var that = this

  var keyboard = this.keyboard = new KeyboardState();
  var clock    = this.clock    = new THREE.Clock();
  var fps      = this.fps      = opts.fps || null;

  // GUI
  var gui = this.gui = new GUI();
      
	// SCENE
	var scene = this.scene = new THREE.Scene();

	var SCREEN_WIDTH = window.innerWidth, SCREEN_HEIGHT = window.innerHeight;
	var VIEW_ANGLE = 45, ASPECT = SCREEN_WIDTH / SCREEN_HEIGHT, NEAR = 0.1, FAR = 20000;

	var camera = this.camera = new THREE.PerspectiveCamera( VIEW_ANGLE, ASPECT, NEAR, FAR);

	scene.add(camera);
	camera.position.set(0,128,200);
  var center = this.center = scene.position.clone();
  center.y = 128; // look straight ahead
	camera.lookAt(center)
  

	if ( Detector.webgl )
		var renderer = this.renderer = new THREE.WebGLRenderer( {antialias:true, alpha: true } );
	else
		var renderer = this.renderer = new THREE.CanvasRenderer(); 
	renderer.setSize(SCREEN_WIDTH, SCREEN_HEIGHT);
	container = document.getElementById( 'ThreeJS' );
	container.appendChild( renderer.domElement );
  // EVENTS
  THREEx.WindowResize(renderer, camera);
//  THREEx.FullScreen.bindKey({ charCode : 'm'.charCodeAt(0) });
	// CONTROLS
	var controls = this.controls = new THREE.OrbitControls( camera, renderer.domElement );
  controls.center.y = center.y
	// STATS

	var stats = this.stats = new Stats();
	stats.domElement.style.position = 'absolute';
	stats.domElement.style.bottom = '0px';
	stats.domElement.style.zIndex = 100;
	container.appendChild( stats.domElement );

  if (typeof opts.init == 'function') {
    opts.init(this)
  }

  this.gui.folder("Camera",function() {
    this.add(that, 'fps', { Slow: 1, Slower: 10, "Full Speed": null } );
    this.add(controls.center,'y').min(0).max(256).step(1);
    this.add(that.camera, 'fov').listen().onChange(function(value) {
      camera.updateProjectionMatrix()
    })
    this.folder('position',function() {
      this.add(that.camera.position, 'x').listen();
      this.add(that.camera.position, 'y').listen();
      this.add(that.camera.position, 'z').listen();
    })
    return true;
  })    
  
  this.animate()

  return this

};

WILLOWBRAE.Theater.prototype = {

	constructor: WILLOWBRAE.Theater,
  
  animate: function ()
  {
    var that = this;
    var f = function () { that.animate() }
    if (this.fps == null) {
      requestAnimationFrame( f )
    } else {
      setTimeout(function () { requestAnimationFrame( f ) },1000 / this.fps)
    }
  	this.render();		
  	this.update();
  },

  render: function () 
  {
  	this.renderer.render( this.scene, this.camera );
  },

  update: function()
  {
//    console.log("X.update")
    this.keyboard.update()

  	if ( this.keyboard.pressed("Z") ) 
  	{	  
      console.log("z pressed")
  		// do something
  	}
	
  	this.controls.update();
  	this.stats.update();
    for(var i in this.children) {
      if (typeof this.children[i].update == 'function') {
        this.children[i].update()
      }
    }

  },


  add: function(e) {
    this.scene.add(e) 
    this.children.push(e)
  },
  
  children: [],

  surfaceCache: {},

  // Public function: adds a cardboard 'card' to the world
  card: function (args) {
      //    console.log(args,surfaceCache)
    var src = args.src || "";
    var size   = args.size   || new THREE.Vector2(128,128)
    var anchor = args.anchor || new THREE.Vector2(size.x * 0.5, size.y * 0.5)
    if (this.surfaceCache[src] && false) {
      var floorMaterial = this.surfaceCache[src];
    } else {
    	var floorTexture = new THREE.ImageUtils.loadTexture( src );
      var floorSpec = args.specular && new THREE.ImageUtils.loadTexture( args.specular );

      function update() {
        this.minFilter = THREE.LinearFilter
        // This is needed for non-POT images
        if (!THREE.Math.isPowerOfTwo(size.x) || THREE.Math.isPowerOfTwo(size.y)) {
          this.minFilter = THREE.LinearFilter
        }
        if (args.repeat) {
          this.wrapS = this.wrapT = THREE.MirroredRepeatWrapping; 
          this.repeat = args.repeat
        }
      }

      update.call(floorTexture)
      if (floorSpec) { update.call(floorSpec) }

    	var floorMaterial = new THREE.MeshBasicMaterial( { 
		        side: THREE.FrontSide /* both sides of this mesh are drawn */ 
      });
    	var backMaterial = new THREE.MeshBasicMaterial( { 
		        side: THREE.BackSide /* both sides of this mesh are drawn */ 
      });
      if ( args.transparent ) { floorMaterial.transparent = args.transparent }

//      floorMaterial.transparent = true;
//      floorMaterial.depthWrite = false // Artifacts also disappear when seting depthTest to false
      floorMaterial.map = floorTexture
      if (floorSpec) { 
        floorMaterial.specularMap = floorSpec
      }
      floorMaterial.needsUpdate = true // This is needed
      backMaterial.wireframe = true
      backMaterial.needsUpdate = true // This is needed

//	    this.surfaceCache[src] = floorMaterial;
    }

    // We always create an anchor, and fasten our surface to this anchor.
    var center = new THREE.Object3D();
    var floorGeometry = new THREE.PlaneGeometry(size.x,size.y,10,10);
    var floor = new THREE.Mesh(floorGeometry, floorMaterial);
    var floorGeometry = new THREE.PlaneGeometry(size.x,size.y,2,2);
    var back = new THREE.Mesh(floorGeometry, backMaterial);

//    console.log("anchor size",anchor,size)
//    console.log((0.5 * size.x) - anchor.x ,- (0.5 * size.y) + anchor.y,0);
    floor.position.set((0.5 * size.x) - anchor.x ,-(0.5 * size.y) + anchor.y,0);
    center.add(floor);
    back.position.set((0.5 * size.x) - anchor.x ,-(0.5 * size.y) + anchor.y,-1);
    center.add(back);

    var sphere = new THREE.SphereGeometry( 5, 4, 4 );
    var material = new THREE.MeshBasicMaterial( { color: 0x000000} );
    mesh = new THREE.Mesh( sphere, material );
    center.add(mesh); 
 
    center.card = floor
    return center;
  }
  


};

