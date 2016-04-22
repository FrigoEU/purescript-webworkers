"use strict";

// module WebWorker
/* global exports, Worker, window, onmessage, postMessage */

exports.supportsWebWorkers = !!Worker;

exports.mkWorker = function mkWorker(path){
  return function(){
    return new Worker(path);
  };
};
exports.onmessageFromWorker = function(worker){
  return function(handler){
    return function(){
      worker.onmessage = function(ev){
        handler(ev)();
      };
    };
  };
};

exports.postMessageToWW = function postMessageToWW(worker){
  return function(data){
    return function(){
      worker.postMessage(data);
    };
  };
};

exports.postMessage = function(a){
  return function(){
    postMessage(a);
  };
};
exports.onmessage = function(a){
  return function(){
    onmessage = function(ev){
      a(ev)();
    };
  };
};
