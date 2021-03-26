module Ia

import PrimIO
import Control.Monad.Trans


data Server =
    MkServer AnyPtr


data Request =
    MkRequest AnyPtr


data Response =
    MkResponse AnyPtr


%foreign "node:lambda:(listener)=>require('http').createServer((req,res)=>listener(req)(res)())"
prim__createServer : (AnyPtr -> AnyPtr -> PrimIO ()) -> PrimIO AnyPtr



createServer : HasIO io => (Request -> Response -> IO ()) -> io Server
createServer handler = 
    map MkServer $ primIO $ prim__createServer (\x => \y =>  toPrim $ handler (MkRequest x) (MkResponse y))


%foreign "node:lambda:(server, port, host, callback)=>server.listen(Number(port), host, callback)"
prim__listen : AnyPtr -> Int -> String -> PrimIO () -> PrimIO ()


listen : HasIO io => Server ->  Int -> String -> IO () -> io ()
listen (MkServer server) port host callback = 
    primIO $ prim__listen server port host (toPrim callback)


%foreign "node:lambda:(response, code)=>response.writeHead(Number(code))"
prim__writeHead : AnyPtr -> Int -> PrimIO ()


writeHead : HasIO io => Response -> Int -> io ()
writeHead (MkResponse response) code = 
    primIO $ prim__writeHead response code


%foreign "node:lambda:(response, data)=>response.end(data)"
prim__end : AnyPtr -> String -> PrimIO ()


end : HasIO io => Response -> String -> io ()
end (MkResponse response) dat =
    primIO $ prim__end response dat


%foreign "javascript:lambda:(callback, delay)=>setTimeout(callback, Number(delay))"
prim__setTimeout : (PrimIO ()) -> Int -> PrimIO ()


setTimeout : HasIO io => IO () -> Int -> io ()
setTimeout callback delay = primIO $ prim__setTimeout (toPrim callback) delay



%foreign "javascript:lambda: x => console.log(x)"
prim__consoleLog : String -> PrimIO ()

consoleLog : HasIO io => String -> io ()
consoleLog x = primIO $ prim__consoleLog x







data Cont : (r : Type) -> (a : Type) -> Type where
     MkCont : ((k : a -> r) -> r) -> Cont r a


data ContT : (r : Type) -> (m : Type -> Type) -> (a : Type) -> Type where
    MkContT : ((k : a -> m r) -> m r) -> ContT r m a



interface Monad m => MonadCont (0 m : Type -> Type) where
  callCC : ((a -> m b) -> m a) -> m a



runContT : ContT r m a -> (a -> m r) -> m r
runContT (MkContT f) k = f k


Monad m => Functor (ContT r m) where
  map f m = MkContT (\k => runContT m (\a => k $ f a))


Monad m => Applicative (ContT r m) where
  f <*> v = MkContT (\k => runContT f $ (\g => runContT v (\a => (k $ g a))))
  pure a = MkContT (\k => k a)


Monad m => Monad (ContT r m) where
  m >>= k = MkContT (\k' => runContT m (\a => runContT (k a) k'))


Monad m => MonadCont (ContT r m) where
  callCC f = MkContT $ \k => runContT (f $ \a => MkContT $ \_ => k a) k




MonadTrans (ContT r) where
    lift m = MkContT (\k => m >>= k)



HasIO (ContT a IO) where
    liftIO = lift


AIO : Type -> Type
AIO a = ContT a IO a



wait : Int -> AIO ()
wait ms =
    MkContT $ \k =>
        setTimeout (k ()) ms


launch : AIO a -> IO a
launch cont =
    runContT cont pure



main : IO ()
main = launch $ do
    server <- createServer \req, res => launch $ do
        consoleLog "Got request"
        writeHead res 200
        wait 1000
        end res "OK"

    listen server 8000 "127.0.0.1" $ launch $ do
        consoleLog "Listening on http://127.0.0.1:8000/"
        wait 1000
        consoleLog "One second passed since listening."
