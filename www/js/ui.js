let basicSnippet = `<div class="futurepanel" style="top: 350px">
<div class="futurepanel__header">
  <h1 class="futurepanel__title">Tipos de meteoritos</h1>
</div>
<div class="futurepanel__body" id="types">
</div>
</div>`


let htmlSnippet = `<div class="futurepanel" style="left: 260px;">
<div class="futurepanel__header">
  <h1 class="futurepanel__title">Meteoritos</h1>
</div>
<div class="futurepanel__body">
    <button class="futurebutton" onclick="changeIndex()">Next spot</button>
</div>
</div>`;

let meteorInfoHTML = `<div class="futurepanel" style="right: 20px;">
<div class="futurepanel__header">
  <h1 class="futurepanel__title">EL METEORITO</h1>
</div>
</div>`;

let htmlTypes = undefined;

let uiElement = document.getElementById("UI")
let paginationElements = Math.min(Math.floor((window.innerHeight / 29) - 16), 15)
let currentPageTypes = 0
let currentPageMeteors = 0
let typeofMeteors = 0

function initUI()
{
    uiElement.innerHTML = basicSnippet;
    setMeteorTypes();
}

function setMeteorTypes()
{
    typesElement = document.getElementById("types");
    for(let i = paginationElements * currentPageTypes; i < paginationElements * currentPageTypes + paginationElements; i++)
    {
        typesElement.innerHTML += `<button class="futurebutton" onclick="selectType(this.id)" id=${i}>${meteor_types[i]}</button><br>`
    }

    typesElement.innerHTML += `<button class="futurebutton_arrow" onclick="paginateTypes(-1)"><i class="fas fa-arrow-left"></i></button>
    <button class="futurebutton_arrow" onclick="paginateTypes(+1)"><i class="fas fa-arrow-right"></i></button>`
}

function paginateTypes(idx)
{
    currentPageTypes = Math.max(0,currentPageTypes += idx);
    uiElement.innerHTML = basicSnippet;
    setMeteorTypes();
}

function paginateElements(idx)
{
    currentPageMeteors = Math.max(0,currentPageMeteors += idx);
    uiElement.innerHTML = basicSnippet;
    setMeteorsHTML();
}

function selectType(id)
{
    typeofMeteors = meteor_types[id]
    console.log(typeofMeteors);
    filterByType(typeofMeteors);
    console.log(meteors_shown);
    currentPageMeteors = 0;
    setMeteorsHTML();
}

function setMeteorsHTML()
{
    let htmlToShow = `<div class="futurepanel" style="top: 350px">
    <div class="futurepanel__header">
      <h1 class="futurepanel__title">Meteoritos de clase ${typeofMeteors}</h1>
    </div>
    <div class="futurepanel__body">`
    // Show the meteors!!

    if(meteors_shown.length>paginationElements)
    {
        for(let i = paginationElements * currentPageMeteors; i < paginationElements * currentPageMeteors + paginationElements; i++)
        {
            htmlToShow += `<button class="futurebutton" onclick="changeIndex(this.id)" id=${i}>${meteors_shown[i].name}</button><br>`
        }
        htmlToShow += `<button class="futurebutton_arrow" onclick="paginateElements(-1)"><i class="fas fa-arrow-left"></i></button>
        <button class="futurebutton_arrow" onclick="paginateElements(+1)"><i class="fas fa-arrow-right"></i></button><br>`
    }
    else
    {
        for(let i = 0; i < meteors_shown.length; i++)
        {
            htmlToShow += `<button class="futurebutton" onclick="changeIndex(this.id)" id=${i}>${meteors_shown[i].name}</button><br>`
        }
    }

    htmlToShow += `<button class="futurebutton--alert" onclick="initUI()">Volver</button></div></div>`

    addMeteoritesAsSpots();

    uiElement.innerHTML = htmlToShow;
}


function setMeteorInfo(idToShow)
{
    setMeteorsHTML();
    console.log(meteors_shown[idToShow])
    let mass = meteors_shown[idToShow].mass / 1000;

    mass = mass.toFixed(2);

    let type = "futuremetric__value--optimal";
    let typemsg = "Low"
    
    if(mass > 10)
    {
        type = "futuremetric__value--warning";
        typemsg = "Med"
    }
    if(mass > 50)
    {
        type = "futuremetric__value--alert";
        typemsg = "High"
    }

    uiElement.innerHTML += `<div class="futurepanel" style="right: 10px;">
    <div class="futurepanel__header">
      <h1 class="futurepanel__title">${meteors_shown[idToShow].name}</h1>
    </div>
    <div class="futurepanel__body">
    <h2 class="futurepanel__title">Tipo</h2>
    <p>${meteors_shown[idToShow].recclass}</p>
    <h2 class="futurepanel__title">Año de descubrimiento</h2>
    <p>${meteors_shown[idToShow].year}</p>
    <h2 class="futurepanel__title">Masa</h2>
    <div class="futuremetric futuremetric--circle">
        <div class="futuremetric__value">${mass}</div>
        <div class="futuremetric__label">kg</div>
    </div>
    <div class="futuremetric futuremetric--circle">
        <div class="futuremetric__value ${type}">${typemsg}</div>
        <div class="futuremetric__label">impacto</div>
    </div>
    </div>
    </div>`;
}