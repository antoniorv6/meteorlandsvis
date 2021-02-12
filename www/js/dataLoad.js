meteor_data = undefined
meteor_types = undefined
meteors_shown = undefined

function load_dataset()
{
    fetch("../Data/meteoritelandings.json")
    .then(res => res.json())
    .then(
        data =>{ 
            meteor_data = data.rows;
            prepare_types();
            initUI();
            init_graphics();
        }
    );
}


function prepare_types()
{
    types = []
    meteor_data.forEach(element => {
        types.push(element.recclass)
    });
    meteor_types = [...new Set(types)];
}

function filterByType(type)
{
    meteors_shown = []
    meteor_data.forEach(element=>{
        if(element.recclass == type)
            meteors_shown.push(element);
    })
}