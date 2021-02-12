let impacts = [];
let maxImpactAmount = 32;
var webglEl = document.getElementById('webgl');
var width  = window.innerWidth,
	height = window.innerHeight;
let materialShader;

var radius   = 0.5,
	segments = 64,
	rotation = 0;  

let x = 0//radius * Math.cos(latRad) * Math.cos(lonRad);
let y = 0//radius * Math.sin(latRad);
let z = 1.5//radius * Math.cos(latRad) * Math.sin(lonRad);

let camX = 0;
let camY = 0;
let camZ = 0;

let dirX = 0;
let dirZ = 0;
let dirY = 0;

let velX = 0;
let velZ = 0;
let velY = 0;


let animating = false;

let scene;
let camera;
let controls;
let renderer;
let sphereCamera;

let highlight_meteor = 1;
let impactValue = 0.5;

let rotationMov = true;

let highlighter = undefined

//Geometry that we might want to control
let clouds;
let earth;

let spots = [];

function init()
{
  window.addEventListener("resize", () => {
    resize(); // your function?
    });
  load_dataset();
}

function resize() {

  camera.aspect = window.innerWidth / window.innerHeight;
  camera.updateProjectionMatrix();
  renderer.setSize( window.innerWidth, window.innerHeight );
  paginationElements = Math.min(Math.floor((window.innerHeight / 29) - 16), 15)
  console.log(paginationElements);
  initUI();

  //render();
}

function changeIndex(id)
{
  highlight_meteor = id;
  setNewHighlight(meteors_shown[highlight_meteor].reclat, 
    meteors_shown[highlight_meteor].reclong)
  setMeteorInfo(id);
  if(rotationMov)
  {
    runTween();
    rotationMov = false;
  }
}

function addMeteoritesAsSpots()
{
  //let particles = new THREE.Geometry();
  //let pMaterial = new THREE.ParticleBasicMaterial({
  //  map:loadTexture('assets/textures/Globe/particle.png'),
  //  color: 0xFFFFFF,
  //  size: 0.1
  //});
  //for(let i = 0; i < 5000; i++)
  //{
  //  let particle = setObjectToLatLon(meteor_data[i].reclat, meteor_data[i].reclong);
  //  particles.vertices.push(particle);
  //}
  //var particleSystem = new THREE.ParticleSystem(
  //  particles,
  //  pMaterial);
 // 
  //scene.add(particleSystem);
}

function setObjectToLatLon(latitude, longitude)
{
  let latRad = latitude * (Math.PI / 180);
  let lonRad = -longitude * (Math.PI / 180);
  let X = radius * Math.cos(latRad) * Math.cos(lonRad);
  let Y = radius * Math.sin(latRad);
  let Z = radius * Math.cos(latRad) * Math.sin(lonRad);

  return new THREE.Vector3(X,Y,Z);
}

function setNewHighlight(latitude, longitude)
{
  let latRad = latitude * (Math.PI / 180);
  let lonRad = -longitude * (Math.PI / 180);
  camX = radius * Math.cos(latRad) * Math.cos(lonRad);
  camY = radius * Math.sin(latRad);
  camZ = radius * Math.cos(latRad) * Math.sin(lonRad);

  x = camera.position.x;
  y = camera.position.y;
  z = camera.position.z;

  highlighter.position.set(camX, camY, camZ);

  runTweenCam();

  animating = true;
}

function scaleValue(value, from, to) {
	var scale = (to[1] - to[0]) / (from[1] - from[0]);
	var capped = Math.min(from[1], Math.max(from[0], value)) - from[0];
	return (capped * scale + to[0]);
}

function init_graphics()
{
  if (!Detector.webgl) 
  {
	  Detector.addGetWebGLMessage(webglEl);
	  return;
	}

  scene = new THREE.Scene();
  scene.fog = new THREE.Fog( 0xcce0ff, 0, 5);
  renderer = new THREE.WebGLRenderer();
  renderer.setSize(width, height);
  scene.background = loadSkybox();
  setCameras()
  addLights();
  createSphereGeometry();

  const geometry = new THREE.SphereGeometry( 0.004, 32, 32 );
  const material = new THREE.MeshBasicMaterial( {color: 0xde2900} );
  highlighter    = new THREE.Mesh( geometry, material );

  scene.add(highlighter);

  setNewHighlight(meteor_data[highlight_meteor].reclat, 
    meteor_data[highlight_meteor].reclong)
  
  highlighter.position.set(0, 0, 0);

	//const axesHelper = new THREE.AxesHelper( 5 );
	//scene.add( axesHelper );
  webglEl.appendChild(renderer.domElement);
  render();

}

function setCameras()
{
  camera = new THREE.PerspectiveCamera(45, width / height, 0.01, 1000);
  camera.position.z = 0.9;
  controls = new THREE.TrackballControls(camera);
  sphereCamera = new THREE.CubeCamera(1,1000,500);
  sphereCamera.position.set(0,1,0);
	scene.add(sphereCamera);
}

function createSphereGeometry()
{
  earth = loadEarth(radius, sphereCamera, new THREE.Vector3().set(camX,camY,camZ))
	scene.add(earth);
  clouds = createClouds(radius, 64)
  scene.add(clouds);
}

function addLights()
{
  scene.add(new THREE.AmbientLight(0x333333));
  var light = new THREE.DirectionalLight(0xffffff, 2);
	light.position.set(5,3,5);
	scene.add(light);
}

function render()
{
  if(animating)
  {
    var camDistance = camera.position.length();
    camera.position.copy({x, y, z}).normalize().multiplyScalar(camDistance);
  }
  if(rotationMov)
  {
    clouds.rotation.y += 0.0005;
  }
  else
    clouds.rotation.y += 0.0001
  controls.update();
  //earth.rotation.y = Math.atan2( ( camera.position.x - x ), ( camera.position.z - z ) )
  //camera.lookAt(x,y,z)
  //console.log(camera.rotation)
	TWEEN.update();		
	renderer.render(scene, camera);
	requestAnimationFrame(render);
}

function runTween() 
{
  var tween = new TWEEN.Tween({value: 0})
    .to({ value: 1 }, 5000)
    //.easing(TWEEN.Easing.Quintic.Out)
    .onUpdate(val => {
      if (materialShader) materialShader.uniforms.impactRatio.value = val.value;
      //console.log(val.value)
    })
    .onComplete((val) => {
      if (materialShader) materialShader.uniforms.impactPosition.value.set(
        camX,
        camY,
       camZ
      );
      runTween();
    });
  tween.start();
}

function runTweenCam()
{
  tween2 = new TWEEN.Tween({x, y, z}).to({x: camX, y: camY, z: camZ}, 3000).easing(TWEEN.Easing.Quadratic.Out)
  .onUpdate(val=>{x = val.x, y= val.y, z=val.z})
  .onComplete((val)=>{animating=false});
  tween2.start();
}