"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[556],{3905:(e,t,r)=>{r.d(t,{Zo:()=>s,kt:()=>m});var n=r(67294);function a(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function o(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,n)}return r}function l(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?o(Object(r),!0).forEach((function(t){a(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):o(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function i(e,t){if(null==e)return{};var r,n,a=function(e,t){if(null==e)return{};var r,n,a={},o=Object.keys(e);for(n=0;n<o.length;n++)r=o[n],t.indexOf(r)>=0||(a[r]=e[r]);return a}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(n=0;n<o.length;n++)r=o[n],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(a[r]=e[r])}return a}var c=n.createContext({}),u=function(e){var t=n.useContext(c),r=t;return e&&(r="function"==typeof e?e(t):l(l({},t),e)),r},s=function(e){var t=u(e.components);return n.createElement(c.Provider,{value:t},e.children)},p="mdxType",f={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},d=n.forwardRef((function(e,t){var r=e.components,a=e.mdxType,o=e.originalType,c=e.parentName,s=i(e,["components","mdxType","originalType","parentName"]),p=u(r),d=a,m=p["".concat(c,".").concat(d)]||p[d]||f[d]||o;return r?n.createElement(m,l(l({ref:t},s),{},{components:r})):n.createElement(m,l({ref:t},s))}));function m(e,t){var r=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var o=r.length,l=new Array(o);l[0]=d;var i={};for(var c in t)hasOwnProperty.call(t,c)&&(i[c]=t[c]);i.originalType=e,i[p]="string"==typeof e?e:a,l[1]=i;for(var u=2;u<o;u++)l[u]=r[u];return n.createElement.apply(null,l)}return n.createElement.apply(null,r)}d.displayName="MDXCreateElement"},52157:(e,t,r)=>{r.r(t),r.d(t,{contentTitle:()=>l,default:()=>p,frontMatter:()=>o,metadata:()=>i,toc:()=>c});var n=r(87462),a=(r(67294),r(3905));const o={},l="Changelog",i={type:"mdx",permalink:"/GreenTea/CHANGELOG",source:"@site/pages/CHANGELOG.md",title:"Changelog",description:"Unreleased",frontMatter:{}},c=[{value:"Unreleased",id:"unreleased",level:2},{value:"0.2.0",id:"020",level:2},{value:"0.1.1",id:"011",level:2},{value:"0.1.0",id:"010",level:2}],u={toc:c},s="wrapper";function p(e){let{components:t,...r}=e;return(0,a.kt)(s,(0,n.Z)({},u,r,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("h1",{id:"changelog"},"Changelog"),(0,a.kt)("h2",{id:"unreleased"},"Unreleased"),(0,a.kt)("p",null,"Nothing yet!"),(0,a.kt)("h2",{id:"020"},"0.2.0"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Catch edge case where a GreenTea constructor is passed into a GreenTea constructor"),(0,a.kt)("li",{parentName:"ul"},"Freeze GreenTea and its child tables so they can't be modified"),(0,a.kt)("li",{parentName:"ul"},"Change ",(0,a.kt)("inlineCode",{parentName:"li"},"basic")," types to have a ",(0,a.kt)("inlineCode",{parentName:"li"},"typeof")," or ",(0,a.kt)("inlineCode",{parentName:"li"},"type")," to differentiate them ",(0,a.kt)("em",{parentName:"li"},"[breaking change]")),(0,a.kt)("li",{parentName:"ul"},"Fix some docs issues")),(0,a.kt)("p",null,"Pushing this is a normal breaking change because no one's using this library yet so there are no ecosystem concerns about a breaking change!"),(0,a.kt)("h2",{id:"011"},"0.1.1"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Fixed formatting for error cases where 3+ errors occur on one line.")),(0,a.kt)("h2",{id:"010"},"0.1.0"),(0,a.kt)("p",null,"Initial release.\nExpect future breaking changes as ergonomics are figured out."))}p.isMDXComponent=!0}}]);