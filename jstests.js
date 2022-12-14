var ex = {value: 10, tag: "R"};

function l(v){
    return {value: v, tag: "L"}
}

function c(x, f1, f2){
    if (x.tag == "L") return f1(x.value)
    else return f2(x.value)
}

function p1(x){return x+1}
function p2(x){return x+2}

console.log(c(ex, p1, p2));
console.log(c(l(10), p1, p2));