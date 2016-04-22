"use strict";

// module WebWorker
/* global exports, Worker, window, addEventListener */

exports.supportsWebWorkers = !!Worker;

exports.mkWorker = function mkWorker(path){
  return function(){
    return new Worker(path);
  };
};
exports.onmessageFromWorker = function(worker){
  return function(handler){
    return function(){
      worker.onmessage = handler;
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
    window.postMessage(a);
  }
};
exports.onmessage = function(a){
  return function(){
    addEventListener("message", a);
  }
};
