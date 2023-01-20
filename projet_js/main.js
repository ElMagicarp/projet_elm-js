const prompt = require('prompt');
const colors = require('@colors/colors/safe');
const exec = require('child_process').exec;

let cli = true;
let userInput


    prompt.message = colors.brightWhite.bgBlue('💨  '+'CLImatiseur'+'   💨'); //mettre des couleurs sympa
    prompt.delimiter = '>';
    prompt.start();

    const promiseResult = new Promise((resolve, reject) => {
        let userInput = prompt.get([{name: 'command',  description:'enter a command'}], (err, result)=> {
            if(err){
                return Error(err);
            }
            else{userInput = result;console.log(userInput);}
            });
        if (userInput) {
            resolve(userInput);
        } else {
            reject(new Error("User input unrecognised!"));
        }
    });
    
    promiseResult
      .then(command(userInput))
      .catch(error => console.log(error.message));
    

// prompt.get([{
//     name: 'command',
//    description:'enter a command'
//}],(err, result)=>{
            
//})
    
function command(result) {
    if(result === 'lp'){
        exec('ps a', (er, stdout, stderr)=>{
            console.log(stdout);
        })
    }
    else if(result === 'stop'){
        cli = false;
        process.exit();
    }
    else if(result === ''){
        console.log('Veulliez vous référer à la documentation du CLI avec la commande help');
    }
}
    
    