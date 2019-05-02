/////////////////////////////////// Q1 ////////////////////////////////

const reduce = require('ramda');
function all(promises) {
  return new Promise((resolve, reject)=> {
    let successArr = new Array(promises.length);
    if (promises.length == 0)
      resolve(successArr);
    let pending = promises.length;
    for (let i = 0; i < promises.length; i++) {
        ((i)=>{
      promises[i].then((result)=> {
        successArr[i] = result;
        pending -= 1;
        if (pending == 0)
          resolve(successArr);
      }).catch((error)=> reject(error));
    })(i);
    }
  });
}

function* naturalNumbers(g) {
    for (let n = g;; n++) {
        yield n;
    }
}

function* take(n, generator) {
    for (let x of generator) {
        if (n <= 0)
            return;
        n--;
        yield x;
    }
}


function* filterGen(generator, filterFunc) {
 for (let x of generator) {
 if (filterFunc(x)) {
 yield x;
 }
 }
}


function* sieve(a) {
     for (let x of a) {
        yield x;  
   let discarder =((x)=>(item) =>{return (item % x)!== 0;})(x);
    yield* sieve(filterGen(a,discarder));
}
}


