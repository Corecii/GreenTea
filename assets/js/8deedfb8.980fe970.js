"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[556],{3905:(e,t,n)=>{n.d(t,{Zo:()=>d,kt:()=>k});var l=n(67294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function r(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);t&&(l=l.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,l)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?r(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):r(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function o(e,t){if(null==e)return{};var n,l,a=function(e,t){if(null==e)return{};var n,l,a={},r=Object.keys(e);for(l=0;l<r.length;l++)n=r[l],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(l=0;l<r.length;l++)n=r[l],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var u=l.createContext({}),p=function(e){var t=l.useContext(u),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},d=function(e){var t=p(e.components);return l.createElement(u.Provider,{value:t},e.children)},s="mdxType",c={inlineCode:"code",wrapper:function(e){var t=e.children;return l.createElement(l.Fragment,{},t)}},m=l.forwardRef((function(e,t){var n=e.components,a=e.mdxType,r=e.originalType,u=e.parentName,d=o(e,["components","mdxType","originalType","parentName"]),s=p(n),m=a,k=s["".concat(u,".").concat(m)]||s[m]||c[m]||r;return n?l.createElement(k,i(i({ref:t},d),{},{components:n})):l.createElement(k,i({ref:t},d))}));function k(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var r=n.length,i=new Array(r);i[0]=m;var o={};for(var u in t)hasOwnProperty.call(t,u)&&(o[u]=t[u]);o.originalType=e,o[s]="string"==typeof e?e:a,i[1]=o;for(var p=2;p<r;p++)i[p]=n[p];return l.createElement.apply(null,i)}return l.createElement.apply(null,n)}m.displayName="MDXCreateElement"},52157:(e,t,n)=>{n.r(t),n.d(t,{contentTitle:()=>i,default:()=>s,frontMatter:()=>r,metadata:()=>o,toc:()=>u});var l=n(87462),a=(n(67294),n(3905));const r={},i="Changelog",o={type:"mdx",permalink:"/GreenTea/CHANGELOG",source:"@site/pages/CHANGELOG.md",title:"Changelog",description:"Unreleased",frontMatter:{}},u=[{value:"Unreleased",id:"unreleased",level:2},{value:"0.4.10",id:"0410",level:2},{value:"0.4.9+pesde",id:"049pesde",level:2},{value:"0.4.9",id:"049",level:2},{value:"0.4.8",id:"048",level:2},{value:"0.4.7",id:"047",level:2},{value:"0.4.6",id:"046",level:2},{value:"0.4.5",id:"045",level:2},{value:"0.4.4",id:"044",level:2},{value:"0.4.3",id:"043",level:2},{value:"0.4.2",id:"042",level:2},{value:"0.4.1",id:"041",level:2},{value:"0.4.0",id:"040",level:2},{value:"0.3.1",id:"031",level:2},{value:"0.3.0",id:"030",level:2},{value:"0.2.1",id:"021",level:2},{value:"0.2.0",id:"020",level:2},{value:"0.1.1",id:"011",level:2},{value:"0.1.0",id:"010",level:2}],p={toc:u},d="wrapper";function s(e){let{components:t,...n}=e;return(0,a.kt)(d,(0,l.Z)({},p,n,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("h1",{id:"changelog"},"Changelog"),(0,a.kt)("h2",{id:"unreleased"},"Unreleased"),(0,a.kt)("p",null,"Nothing yet!"),(0,a.kt)("h2",{id:"0410"},"0.4.10"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Handle missing ",(0,a.kt)("inlineCode",{parentName:"li"},"utf8.graphemes")," function for Lune support."),(0,a.kt)("li",{parentName:"ul"},"Added lune support.")),(0,a.kt)("h2",{id:"049pesde"},"0.4.9+pesde"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Added pesde support. No code changes or package version changes.")),(0,a.kt)("h2",{id:"049"},"0.4.9"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Remove class which was removed from the engine."),(0,a.kt)("li",{parentName:"ul"},"Add docs for what to do when a class is removed from the engine.")),(0,a.kt)("h2",{id:"048"},"0.4.8"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Optimizations"),(0,a.kt)("li",{parentName:"ul"},"Internal change: only use ",(0,a.kt)("inlineCode",{parentName:"li"},"\\")," in markdown comments for indication newlines. Cleans up moonwave docs some."),(0,a.kt)("li",{parentName:"ul"},"Internal change: wrap moonwave in a helper script that makes corrections for moonwave."),(0,a.kt)("li",{parentName:"ul"},"Internal change: format with Stylua.")),(0,a.kt)("h2",{id:"047"},"0.4.7"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Fix Type:assert and add tests for it"),(0,a.kt)("li",{parentName:"ul"},"Fix tests for table type to check __index")),(0,a.kt)("h2",{id:"046"},"0.4.6"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Fix buggy type definition for ",(0,a.kt)("inlineCode",{parentName:"li"},"GreenTea.fn")),(0,a.kt)("li",{parentName:"ul"},"Make ",(0,a.kt)("inlineCode",{parentName:"li"},"GreenTea.build")," handle tuples properly"),(0,a.kt)("li",{parentName:"ul"},"Export a type for ",(0,a.kt)("inlineCode",{parentName:"li"},"GreenTea.build"),'\'s "BuiltType" values')),(0,a.kt)("h2",{id:"045"},"0.4.5"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Fix type signature of ",(0,a.kt)("inlineCode",{parentName:"li"},"__call")),(0,a.kt)("li",{parentName:"ul"},"Fix typechecking failure in ",(0,a.kt)("inlineCode",{parentName:"li"},".meta"))),(0,a.kt)("h2",{id:"044"},"0.4.4"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Fix: meta did not return input")),(0,a.kt)("h2",{id:"043"},"0.4.3"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Add ",(0,a.kt)("inlineCode",{parentName:"li"},"meta")," to exports")),(0,a.kt)("h2",{id:"042"},"0.4.2"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Make metadata type more permissive")),(0,a.kt)("h2",{id:"041"},"0.4.1"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Add user-specified metadata")),(0,a.kt)("h2",{id:"040"},"0.4.0"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Change behavior around tables to respect ",(0,a.kt)("inlineCode",{parentName:"li"},"__iter"),", ",(0,a.kt)("inlineCode",{parentName:"li"},"__index")),(0,a.kt)("li",{parentName:"ul"},"Change behavior around tables to not check if `",(0,a.kt)("inlineCode",{parentName:"li"},"getmetatable(input) == getmetable(typedef)"))),(0,a.kt)("h2",{id:"031"},"0.3.1"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Only show simplified input types in error messages")),(0,a.kt)("h2",{id:"030"},"0.3.0"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Change ",(0,a.kt)("inlineCode",{parentName:"li"},"Type.__call")," to return a string cause instead of an object for ",(0,a.kt)("inlineCode",{parentName:"li"},"t")," and ",(0,a.kt)("inlineCode",{parentName:"li"},"assert")," compatibility.")),(0,a.kt)("h2",{id:"021"},"0.2.1"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Add missing ",(0,a.kt)("inlineCode",{parentName:"li"},"t")," members")),(0,a.kt)("h2",{id:"020"},"0.2.0"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Catch edge case where a GreenTea constructor is passed into a GreenTea constructor"),(0,a.kt)("li",{parentName:"ul"},"Freeze GreenTea and its child tables so they can't be modified"),(0,a.kt)("li",{parentName:"ul"},"Change ",(0,a.kt)("inlineCode",{parentName:"li"},"basic")," types to have a ",(0,a.kt)("inlineCode",{parentName:"li"},"typeof")," or ",(0,a.kt)("inlineCode",{parentName:"li"},"type")," to differentiate them ",(0,a.kt)("em",{parentName:"li"},"[breaking change]")),(0,a.kt)("li",{parentName:"ul"},"Fix some docs issues")),(0,a.kt)("p",null,"Pushing this is a normal breaking change because no one's using this library yet so there are no ecosystem concerns about a breaking change!"),(0,a.kt)("h2",{id:"011"},"0.1.1"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Fixed formatting for error cases where 3+ errors occur on one line.")),(0,a.kt)("h2",{id:"010"},"0.1.0"),(0,a.kt)("p",null,"Initial release.\nExpect future breaking changes as ergonomics are figured out."))}s.isMDXComponent=!0}}]);