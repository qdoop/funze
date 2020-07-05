

// zlog(document.getElementById("tmpl_app__00"))
let cccblock=Vue.component('cccblock',{ template:"<div> sample ccc block</div>" })

let AppComponenent={
    template:document.getElementById("tmpl_app__00").innerHTML,
    methods:{
        clickHandler(name){
            // zlog(name)
            this.$root.$data.target=name
            UpdateTarget()
            document.getElementById("engn_status__00").style.display='block'
        },
        overHandler(name){
            // zlog(name)
            this.$root.$data.target=name
            UpdateTarget()
        }
    }
}
let App=Vue.component('App', AppComponenent)

window.vue = new Vue({
    el: '#app',
    // router,
    // components: { App, cccblock },
    template: '<App/>',
    data: {
        target:'',
        engine:{},
        engns:[],
        thebus: new Vue(),
        thedat: 'thedat'
    }
})


function fetchApiData(urlpath, timeout, cb) {
    var xhr = new XMLHttpRequest()
    xhr.addEventListener("load", function () {
        var rdata = this.responseText  //IMPORTANT
        // console.log(this.responseText)
        try{    
            let rs=JSON.parse(rdata) 
            cb(null, rs)            
        }
        catch(ex){                    
            txt="JSON PARSE ERROR\n;"
        }                
        setTimeout(function () {
            fetchApiData(urlpath, timeout, cb)
        }, timeout);
    })
    xhr.addEventListener("error", function (error_data) {
        setTimeout(function () {
            fetchApiData(urlpath, timeout, cb)
        }, timeout);
    })
    var endp = window.localStorage.getItem('CAYLEY_ENDPOINT') || "http://localhost:8080";
    // var endp = "http://localhost:8080";
    xhr.open("GET", endp + urlpath);
    xhr.send();  //trick not to crash with empty data
}
fetchApiData('/api/v1/engines', 2000, function(err, rs){
    // zlog(rs)
    vue.$data.engns=rs
})



function UpdateTarget1(){
    var xhr = new XMLHttpRequest()
    xhr.addEventListener("load", function () {
        var rdata = this.responseText  //IMPORTANT
        // console.log(this.responseText)
        try{    
            let rs=JSON.parse(rdata) 
            // cb(null, rs)
            if((''+vue.$data.target)==rs.name){ //use prop value
                vue.$data.engine=rs
            }            
        }
        catch(ex){                    
            txt="JSON PARSE ERROR\n;"
        }
        setTimeout(UpdateTarget1, 4000);
    })
    xhr.addEventListener("error", function (error_data) {
        setTimeout(UpdateTarget1, 4000);
    })
    if (vue.$data.target){
        var endp = window.localStorage.getItem('CAYLEY_ENDPOINT') || "http://localhost:8080";
        // var endp = "http://localhost:8080";
        xhr.open("GET", endp + '/api/v1/engine/' + vue.$data.target );
        xhr.send();  //trick not to crash with empty data
    }
    else{
        setTimeout(UpdateTarget1, 4000);
    }
}
UpdateTarget1()

function UpdateTarget(){
    var xhr = new XMLHttpRequest()
    xhr.addEventListener("load", function () {
        var rdata = this.responseText  //IMPORTANT
        // console.log(this.responseText)
        try{    
            let rs=JSON.parse(rdata) 
            // cb(null, rs)
            if((''+vue.$data.target)==rs.name){ //use prop value
                vue.$data.engine=rs
            }            
        }
        catch(ex){                    
            txt="JSON PARSE ERROR\n;"
        }
    })
    if (vue.$data.target){
        var endp = window.localStorage.getItem('CAYLEY_ENDPOINT') || "http://localhost:8080";
        // var endp = "http://localhost:8080";
        xhr.open("GET", endp + '/api/v1/engine/' + vue.$data.target );
        xhr.send();  //trick not to crash with empty data
    }
}
