window.zlog = console.log;





function changeEndpoint(endpoint) {
    window.localStorage.setItem('CAYLEY_ENDPOINT', endpoint)
    document.querySelector('#ctrl_endpoint__00').value = endpoint
    if ("http://localhost:49210" == endpoint) {
        document.querySelector('.menubar').style.background = "red"
    }
    else {
        document.querySelector('.menubar').style.background = "white"
    }
}
changeEndpoint(window.localStorage.getItem('CAYLEY_ENDPOINT') || "http://localhost:64210")
document.querySelector('#ctrl_endpoint__00').addEventListener('change', function (event) {
    console.log('changed')
    changeEndpoint(this.value)
})




var s = null;
var group = null;
var neutralColor = "#999999"
var green = "#0F9D58"


var defaultQueryStrings = {
    "gizmo": "zall \/  f:has / f:color   / zeval", //"g.Emit('Hello World')",
    "gremlin": "g.Emit('Hello World')",
    "mql": "[{\n  \"id\": \"Hello World\"\n}]",
    "graphql": "{\n  nodes(first: 10){\n    id\n  }\n}"
}

var getLastQueryStringFor = function (type) {
    if (typeof (Storage) !== "undefined") {
        return localStorage.getItem("cayleySavedQueries" + type)
    } else {
        return defaultQueryStrings[type]
    }
}

var switchTo = function (type) {
    if (type === "gizmo") { switchToGizmo() }
    if (type === "gremlin") { switchToGremlin() }
    if (type === "mql") { switchToMQL() }
    if (type === "graphql") { switchToGraphQL() }
    if (typeof (Storage) !== "undefined") {
        localStorage.setItem("cayleyQueryLang", type);
    }
    if (window.editor) {
        editor.setValue(getLastQueryStringFor(type))
    }
}

var switchToGizmo = function () {
    // $("#selected-query-lang").html("Gizmo " + caretSpan)
    selectedQueryLanguage = "gizmo"
}

var switchToGremlin = function () {
    // $("#selected-query-lang").html("Gremlin " + caretSpan)
    selectedQueryLanguage = "gremlin"
}

var switchToMQL = function () {
    // $("#selected-query-lang").html("MQL" + caretSpan)
    selectedQueryLanguage = "mql"
}

var switchToGraphQL = function () {
    // $("#selected-query-lang").html("GraphQL" + caretSpan)
    selectedQueryLanguage = "graphql"
}

selectedQueryLanguage = "gizmo"
var caretSpan = " &nbsp <span class='caret'></span>"

if (typeof (Storage) !== "undefined") {
    savedQueries = localStorage.getItem("cayleySavedQueriesgraphql");
    if (savedQueries === null) {
        for (var key in defaultQueryStrings) {
            localStorage.setItem("cayleySavedQueries" + key, defaultQueryStrings[key])
        }
    }
    lang = localStorage.getItem("cayleyQueryLang");
    if (lang !== null) {
        switchTo(lang)
    } else {
        switchTo("gizmo")
    }
} else {
    switchTo("gizmo")
}



document.getElementById("gizmo-dropdown").addEventListener('click', function () {
    switchTo("gizmo")
})


document.getElementById("gremlin-dropdown").addEventListener('click', function () {
    switchTo("gremlin")
})


document.getElementById("mql-dropdown").addEventListener('click', function () {
    switchTo("mql")
})


document.getElementById("graphql-dropdown").addEventListener('click', function () {
    switchTo("graphql")
})





function beep() {
    var snd = new Audio("data:audio/wav;base64,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qJrx4KOQjahZxWbcZgztj2c49nKmkId44S71j0c8eV9yDK6uPRzx5X18eDvjvQ6yKo9ZSS6l//8elePK/Lf//IInrOF/FvDoADYAGBMGb7FtErm5MXMlmPAJQVgWta7Zx2go+8xJ0UiCb8LHHdftWyLJE0QIAIsI+UbXu67dZMjmgDGCGl1H+vpF4NSDckSIkk7Vd+sxEhBQMRU8j/12UIRhzSaUdQ+rQU5kGeFxm+hb1oh6pWWmv3uvmReDl0UnvtapVaIzo1jZbf/pD6ElLqSX+rUmOQNpJFa/r+sa4e/pBlAABoAAAAA3CUgShLdGIxsY7AUABPRrgCABdDuQ5GC7DqPQCgbbJUAoRSUj+NIEig0YfyWUho1VBBBA//uQZB4ABZx5zfMakeAAAAmwAAAAF5F3P0w9GtAAACfAAAAAwLhMDmAYWMgVEG1U0FIGCBgXBXAtfMH10000EEEEEECUBYln03TTTdNBDZopopYvrTTdNa325mImNg3TTPV9q3pmY0xoO6bv3r00y+IDGid/9aaaZTGMuj9mpu9Mpio1dXrr5HERTZSmqU36A3CumzN/9Robv/Xx4v9ijkSRSNLQhAWumap82WRSBUqXStV/YcS+XVLnSS+WLDroqArFkMEsAS+eWmrUzrO0oEmE40RlMZ5+ODIkAyKAGUwZ3mVKmcamcJnMW26MRPgUw6j+LkhyHGVGYjSUUKNpuJUQoOIAyDvEyG8S5yfK6dhZc0Tx1KI/gviKL6qvvFs1+bWtaz58uUNnryq6kt5RzOCkPWlVqVX2a/EEBUdU1KrXLf40GoiiFXK///qpoiDXrOgqDR38JB0bw7SoL+ZB9o1RCkQjQ2CBYZKd/+VJxZRRZlqSkKiws0WFxUyCwsKiMy7hUVFhIaCrNQsKkTIsLivwKKigsj8XYlwt/WKi2N4d//uQRCSAAjURNIHpMZBGYiaQPSYyAAABLAAAAAAAACWAAAAApUF/Mg+0aohSIRobBAsMlO//Kk4soosy1JSFRYWaLC4qZBYWFRGZdwqKiwkNBVmoWFSJkWFxX4FFRQWR+LsS4W/rFRb/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////VEFHAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAU291bmRib3kuZGUAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMjAwNGh0dHA6Ly93d3cuc291bmRib3kuZGUAAAAAAAAAACU=");
    snd.play();
}
//beep();


document.getElementById("output__00").addEventListener('dblclick', function (evt) {
    //'focus', function (evt) {
    zlog(evt)
    //trick to automate sending code
    document.getElementById("run_button").click()
})


document.getElementById("repl__00").addEventListener('keydown', function (evt) {
    // zlog(evt)
    if (13 != evt.keyCode) return;

    

    evt.preventDefault();

    var ctrl = document.getElementById("repl__00");

    var bclr = ctrl.style.borderColor;
    ctrl.style.borderColor = "red";
    // let val = ctrl.value;
    // ctrl.value = val //tric to put caret at the end
    let val=window.repl.getValue();
    // alert(val);

    var xhr = new XMLHttpRequest();
    xhr.addEventListener("load", function () { 
        ctrl.style.borderColor = "green";
        window.repl.setValue(''); 
    });
    var endp = window.localStorage.getItem('CAYLEY_ENDPOINT') || "http://localhost:8080";
    // var endp = "http://localhost:8080";
    xhr.open("POST", endp + "/api/v1/repl/0");
    xhr.send(' ' + val);  //trick not to crash with empty data
})



function handleFileSelect(evt) {
    let files = evt.target.files
    // alert(files)
    let reader = new FileReader()
    reader.addEventListener("load", function () {
        window.editor.setValue(reader.result)
//        alert(reader.result)
    }, false)
    files[0] && reader.readAsText(files[0])
}

    /////////////////////////////////////////// SEnD THE CODE TO SERVER
document.getElementById("run_button").addEventListener('click', function () {
    var data = window.editor.getValue()
    zlog(data);
    var start_time = Date.now()

    document.title = 'QUERY RUNNING - cayley';
    document.body.style.cursor = 'wait';
    //beep();

    //alert("ssssssss")
   const octl = document.getElementById('output__00');
   octl.value = '\n\n__START_____________\n';


    try {
        var xhr = new XMLHttpRequest()
        xhr.addEventListener("load", function () {

            var return_data = this.responseText  //IMPORTANT
            // console.log(this.responseText)

           document.querySelector('#statusbar__00').innerHTML =
               (new Date()).toISOString() + ' DONE elapsed ' + (Date.now() - start_time) + 'ms'

            if (typeof (Storage) !== "undefined") {
                localStorage.setItem("cayleySavedQueries" + selectedQueryLanguage, data)
            }


            //return_data = return_data.replace(/\\u003c/g, '<').replace(/\\u003e/g, '>')


            // document.getElementById('output').value = return_data
            //     // stopAndReset();

            // const octl = document.getElementById('output');
            // octl.value = octl.value + "\nREADY____________\n";

            document.title = 'DONE - cayley';
            document.body.style.cursor = 'default';



        });
        xhr.addEventListener("progress", function (oEvent) {
            if (oEvent.lengthComputable) {
                //document.getElementById('output').value = 'progress=' + oEvent.loaded / oEvent.total;
                // ...
            } else {
                // Unable to compute progress information since the total size is unknown
                // document.getElementById('output').value = 'progress=...'+ oEvent.loaded
            }
           // console.log(oEvent)
        })
        xhr.addEventListener("error", function (error_data) {
            // document.getElementById('output').value = error_data
            document.title = 'DONE - cayley';
            document.body.style.cursor = 'default';
        })
        xhr.addEventListener("abort", function (abort_data) {
            // document.getElementById('output').value = abort_data
            document.title = 'DONE - cayley';
            document.body.style.cursor = 'default';
        })
        xhr.open("POST", localStorage.getItem('CAYLEY_ENDPOINT') + "/api/v1/runs/0");
        xhr.send(' ' + data);  //trick not to crash with empty data

        document.querySelector('#statusbar__00').innerHTML = (new Date()).toISOString() + ' started...'

    } catch (err) {

        alert(err)

    }
})

///////////////////////////////////////////////RESULTS LOADER
window.conn_status = false;
function LoadResults() {
    // zlog("log!")            
    var xhr = new XMLHttpRequest()
    xhr.addEventListener("load", function () {
        const octl = document.getElementById('output__00');
        if (false == window.conn_status) {
            window.conn_status = true;
            octl.value = octl.value + "\nCONNECTION ok!\n"
            //beep();
        }

        var rdata = this.responseText  //IMPORTANT
        // console.log(this.responseText)

        let txt = ""
        try {
            let rs = JSON.parse(rdata)
            for (var i = 0; i < rs.length; i++) {
                let r = rs[i]
                if (r.t) {
                    txt = txt + r.v;
                    //r.z + r.k + r.v;
                }
                // txt = txt + "y";
            }
        }
        catch (ex) {
            txt = "JSON PARSE ERROR\n;"
        }
        // zlog(txt)

        const keep = 10000
        octl.value =
            (2 * keep < octl.value.length ? octl.value.substring(keep) : octl.value) + txt;

        setTimeout(function () {
            if (0 < txt.length) {
                octl.scrollTop = 10000;
            }
        }, 100);

        setTimeout(LoadResults, 1000);
    })
    xhr.addEventListener("error", function (error_data) {
        document.getElementById('output__00').value = error_data
        // document.title = 'DONE - cayley';
        // document.body.style.cursor = 'default';
        // alert("no connection!");
        const octl = document.getElementById('output__00');
        octl.value = octl.value + "\nCONNECTION LOST!!!\n"
        window.conn_status = false;
        setTimeout(LoadResults, 4000);
    })
    var endp = window.localStorage.getItem('CAYLEY_ENDPOINT') || "http://localhost:8080";
    // var endp = "http://localhost:8080";
    xhr.open("GET", endp + "/api/v1/logs/0");
    xhr.send();  //trick not to crash with empty data
}
LoadResults();





document.getElementById('files__00').addEventListener('change', handleFileSelect, false);

function handleSaveAs() {

    localStorage.setItem('monaco_editor_codetext',window.editor.getValue());

    let blob = new Blob(['' + window.editor.getValue()], { type: "text/plain;charset=utf-8" })
    window.saveAs(blob, "gizmo_" + (new Date()).toISOString().replace(/[-:]/g, '').substring(0, 15) + ".txt") //requires FIleSaver.js
}
document.getElementById('btn_save_as__00').addEventListener('click', handleSaveAs, false);








require.config({ paths: { 'vs': '/standalone/third_party/monaco-editor/min/vs' } });
require(['vs/editor/editor.main'], function () {

    monaco.languages.typescript.javascriptDefaults.setDiagnosticsOptions({
        // noSemanticValidation: true,
        // noSyntaxValidation: true
    })

    if (localStorage.getItem('monaco_editor_codetext')) {
        localStorage.getItem('monaco_editor_codetext')
    }

    
    // PLEASE NOTE (F1 key) Develop:tokens allows inspection of tokens
    monaco.editor.defineTheme('myCustomTheme', {
        base: 'vs-dark', // can also be vs-dark or hc-black
        inherit: true, // can also be false to completely replace the builtin rules
        rules: [
            // { token: 'comment', foreground: 'ffa500', fontStyle: 'italic underline' },
            // { token: 'comment.js', foreground: '008800', fontStyle: 'bold' },
            { token: 'comment.css', foreground: '0000ff' }, // will inherit fontStyle from `comment` above
            // { token: 'delimiter.parenthesis.js', foreground: '0000ff' } // will inherit fontStyle from `comment` above
            // { token: 'identifier.js', foreground: '9cdcfe' } // will inherit fontStyle from `comment` above

            { token: 'number.fs', foreground: '00ff7f' },
            { token: 'delimiter.fs', foreground: '008b8b' }, //, fontStyle: 'bold'  },
            { token: 'identifier.fs', foreground: '008b8b' }
        ]
    });

    window.FsharpCompleteProvider={
        triggerCharacters:["z"],
        provideCompletionItems(model, position, context, token){

            zlog("provideCompletionItems");

            return[
                    {label:"zadd", kind:0, detail:null,documentation:null,sortText:null,filterText:null,insertText:"null",range:null,textEdit:null},
                    // {label:"zminus", kind:0},
                ];
        }

    };

    monaco.languages.registerCompletionItemProvider('fsharp', window.FsharpCompleteProvider);





    window.editor = monaco.editor.create(document.getElementById('code__00'), {
        // renderWhitespace: 'all',
        // base: 'vs-dark',
        theme: "myCustomTheme",
        // language: 'javascript',
        // language: 'typescript',
        language: "fsharp",
        fontSize: 14,
        fontFamily: "'Courier New', monospace, Consolas, 'Courier New', monospace, 'Segoe UI Emoji'",
        //value: '',
        value:(localStorage.getItem('monaco_editor_codetext'))?localStorage.getItem('monaco_editor_codetext'):'', 
    })


    //ERROR MARKERS SEE PINEZA monaco node module

    function code_error_markers(){
        var regexx=/[#]error[=](\d+),(\d+),(\d+),(\d+)[|](.*)/g
        // var res=regexx.exec("sssssssss\r\n#error=11,12,13,14|xxxxx\r\n vvvvvvvv")
        res=regexx.exec(document.getElementById('output__00').value);
        // zlog(res);
     
        // zlog(models);
        // var model=monaco.editor.getModel();
        // zlog(model);//.getModelMarkers());
        var models=monaco.editor.getModels();
        if(res){            
            monaco.editor.setModelMarkers(models[0],"fsharp",[
                {
                    code: "",
                    severity: 3, //error
                    message: res[5],
                    source: "",
                    startLineNumber: Number(res[1]),
                    startColumn: Number(res[2]),
                    endLineNumber: Number(res[3]),
                    endColumn: Number(res[4])
                }]);
            }
        else{
            monaco.editor.setModelMarkers(models[0],"fsharp",[
                {
                    code: "",
                    severity: 0, //ignore
                    message: "",
                    source: "",
                    startLineNumber: 1,
                    startColumn: 1,
                    endLineNumber: 1000,
                    endColumn: 1
                }]);
            }
        
        setTimeout(code_error_markers,2000);
    } 
    setTimeout(code_error_markers,2000);
    // setTimeout(function(){
    //     var models=monaco.editor.getModels()
    //     zlog(models);
    //     // var model=monaco.editor.getModel();
    //     // zlog(model);//.getModelMarkers());
    //     zlog(monaco.editor.setModelMarkers(models[0],"fsharp",[
    //         {
    //             code: "string xxx",
    //             severity: 0, //cleanup
    //             message: "string xxxx",
    //             source: "stringxxxxxxxxxx xxx\n",
    //             startLineNumber: 10,
    //             startColumn: 1,
    //             endLineNumber: 11,
    //             endColumn: 2
    //         }
    //     ]));
    // }, 5000);

    window.repl = monaco.editor.create(document.getElementById('repl__00'), {
        // renderWhitespace: 'all',
        // base: 'vs-dark',
        theme: "myCustomTheme",
        // language: 'javascript',
        // language: 'typescript',
        language: "fsharp",
        fontSize: 14,
        fontFamily: "'Courier New', monospace, Consolas, 'Courier New', monospace, 'Segoe UI Emoji'",
        value: ''
    })
});
