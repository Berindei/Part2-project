class Channel{
    constructor(){
        this.state = {state: "new"}
    }
    get(k){
        if (this.state.state=="new"){
            this.state = {state: "cb", cb: k}
        }
        else if (this.state.state=="value"){
            v = this.state.v
            this.state = {state:"done"}
            k(v);
        }
    }
    put(v){
        if (this.state.state=="new"){
            this.state = {state: "value", v: v}
        }
        else if (this.state.state=="cb"){
            cb = this.state.cb;
            this.state = {state: "done"}
            cb(v);
        }
    }
}