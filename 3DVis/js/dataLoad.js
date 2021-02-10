meteor_data = undefined

function load_dataset()
{
    fetch("../Data/meteoritelandings.json")
    .then(res => res.json())
    .then(
        data =>{ 
            meteor_data = data.rows;
            console.log(meteor_data);
            init_graphics();
        }
    );
}