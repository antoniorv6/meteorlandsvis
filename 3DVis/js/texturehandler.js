
const textureLoader = new THREE.TextureLoader();

function loadSkybox()
{
    let urls = [
        'assets/textures/Skybox/right.png', 'assets/textures/Skybox/left.png',
        'assets/textures/Skybox/top.png', 'assets/textures/Skybox/bottom.png',
        'assets/textures/Skybox/front.png', 'assets/textures/Skybox/back.png',
      ];
    
    let loader = new THREE.CubeTextureLoader();

    return loader.load(urls);
}

function loadTexture(textureRoute)
{
    return textureLoader.load(textureRoute)
}