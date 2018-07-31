"use strict";

// module WebWorker
/* global exports, Worker, window, onmessage, postMessage */

// exports.supportsWebWorkers = (typeof window !== "undefined" && window.Worker) ? true : false;

exports.mkWorker = function mkWorker(path){
  return function(){
    return new Worker(path);
  };
};
exports.terminateWorker = function terminateWorker(worker){
  return function(){
    worker.terminate();
  };
};
exports.onmessageFromWorker = function(worker){
  return function(f){
    return function(){
      worker.onmessage = function(ev){
        f(ev)();
      };
    };
  };
};

exports.postMessageToWorker = function postMessageToWorker(worker){
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
exports.onmessage = function(f){
  return function(){
    onmessage = function(ev){
      f(ev)();
    };
  };
};
