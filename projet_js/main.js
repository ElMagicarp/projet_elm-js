const prompt = require('prompt');
const colors = require('@colors/colors/safe');
const exec = require('child_process').exec;
const fs = require('fs')
let cli = true;
let userInput
const readline = require('readline');
readline.emitKeypressEvents(process.stdin);
process.stdin.setRawMode(false);


main();

async function main() {
    while (true){
        prompt.message = colors.brightWhite.bgBlue('💨  '+'CLImatiseur'+'   💨'); //mettre des couleurs sympa
        prompt.delimiter = '>';
        prompt.start();
        process.stdin.on('keypress', (str, key) => {
            if (key.ctrl && key.name === 't') {
              console.log("exiting")
              process.exit();
            } 
        });
        var input = await prompt.get([{
            name: 'command',
            description:'enter a command'
        }])

        await command(input);
    }
}
    
async function command(result) {
    if(result.command == 'lp'){
        let a = await lp()
    }
    else if(result.command.split(' ')[0] == 'exe'){
        let a = await exe(result.command)
    }
    else if(result.command == 'stop'){
        cli = false;
        process.exit();
    }
    else if (result.command.split(' ')[0] == 'man'){
        let a = await man(result.command);
    }
    else if (result.command.split(' ')[0] == 'bing'){
        let a = await bing(result.command);
    }
    else {
        console.log('Veulliez vous référer à la documentation du CLI avec la commande man');
    }
}

async function lp (){
    return new Promise((resolve, reject) => {
        exec('ps a', (er, stdout, stderr)=>{
            resolve (console.log(stdout));
        });
    }); 
}

async function exe (command){
    if (command.endsWith("!")){
        if (command.includes('.js')){
            exec('node '+command.substring(4,command.length-1), (er, stdout, stderr)=>{
                console.log("\n");
                console.log(stdout);
                }
            )
        }
        else if (command.includes('.py')){
            exec('python3 '+command.substring(4,command.length-1), (er, stdout, stderr)=>{
                console.log("\n");
                console.log(stdout);      
                }
            )
        }
        else{
            exec(command.substring(4,command.length-1), (er, stdout, stderr)=>{
                }
            )
        }
    }
    else{
        return new Promise((resolve, reject) => {
            if (command.includes('.js')){
                exec('node '+command.substring(4), (er, stdout, stderr)=>{
                    resolve(console.log(stdout));
                    }
                );
            }
            else if (command.includes('.py')){
                exec('python3 '+command.substring(4), (er, stdout, stderr)=>{
                    resolve(console.log(stdout));
                    }
                );
            }
            else {
                exec(command.substring(4), (er, stdout, stderr)=>{
                    resolve(console.log(stdout));
                    }
                );
            }
        }); 
    }
}

async function man (input){
    if (input.split(' ')[1] == null){
        return new Promise((resolve, reject) => {
            let man = input+".txt";
            fs.readFile(man, (err, data) => {
                if (err) throw err;
                resolve (console.log(data.toString()));
                })
        }); 
    }else if (input.split(' ')[1] == "exe" || input.split(' ')[1] == "lp" || input.split(' ')[1] == "stop"){
        return new Promise((resolve, reject) => {
            let man = input.split(' ')[1]+".txt";
            fs.readFile(man, (err, data) => {
                if (err) throw err;
                resolve (console.log(data.toString()));
                })
        }); 
    }else {
        console.log("Veuillez entrer une commande valide à inspecter")
    }
}

async function bing (command){
    return new Promise((resolve, reject) => {
        if (command.includes('-k')){
            exec('kill -KILL '+command.split(' ')[2], (er, stdout, stderr)=>{
                resolve(console.log(stdout));
                }
            );
        }
        else if (command.includes('-p')){
            exec('kill -STOP '+command.split(' ')[2], (er, stdout, stderr)=>{
                resolve(console.log(stdout));
                }
            );
        }
        else if (command.includes('-c')){
            exec('kill -CONT '+command.split(' ')[2], (er, stdout, stderr)=>{
                resolve(console.log(stdout));
                }
            );
        }
    }); 
}
  

