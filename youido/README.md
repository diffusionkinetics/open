Youido
=============

Youido is a high level framework for developing Haskell web applications that satisfy a number of highly restrictive assumptions:

1. You are building enterprise style, not consumer facing, applications.
2. You don't care about SEO ranking or what the URL looks like.
3. Your functionality is guarded by a user login
4. All rendering is done server-side

Within these assumptions, youido attempts to combine what we see as the best aspects of various Haskell
web frameworks. Request handling is performed by pattern matching on algebraic data types like in Yesod, 
but these datatypes are not a single type, allowing composition and reuse.

The main API is given in [Youido.Types](https://github.com/diffusionkinetics/open/blob/master/youido/lib/Youido/Types.hs) 
and an [example is given](https://github.com/diffusionkinetics/open/blob/master/youido/examples/Example.hs)

The principal mechanism of request handling is pattern matching on types that implement the `FromRequest` typeclass. 

Dashboards defined with [dashdo](https://github.com/diffusionkinetics/open/tree/master/dashdo) can 
also be connected to a youido application. This is illustrated in the example.
