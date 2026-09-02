package okay.netty

import okay.*
import okay.given
import okay.http.{Body, Frame, Http, Method, Request, Response, Socket, Sockets}

import io.netty.bootstrap.{Bootstrap, ServerBootstrap}
import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.channel.{Channel as NettyChannel, *}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.{NioServerSocketChannel, NioSocketChannel}
import io.netty.handler.codec.http.*
import io.netty.handler.codec.http.websocketx.*
import io.netty.util.CharsetUtil

import java.net.URI
import java.nio.charset.StandardCharsets.UTF_8
import scala.jdk.CollectionConverters.*

/**
 * Netty behind okay-http's two seams.
 *
 * This is interop in the sense `okay-fs2` and `okay-zio` are: not a
 * better implementation than the JDK's, an implementation in a runtime
 * someone is already running. It is also the honest answer to "NIO with
 * HTTP" — `okay.http.Nio` is the byte level because hand-rolling
 * HTTP/1.1 is work without a payoff, and Netty is where that codec is
 * already written, which is what makes it worth a dependency.
 *
 * Everything owns an event loop group, so everything is a `Resource`.
 */
object Netty {

  /** the shared loops — one group, released with the scope */
  private def group(): NioEventLoopGroup ! Resource =
    Resource.acquire(NioEventLoopGroup())(g => { g.shutdownGracefully(); () })

  // ---- the client seam

  def http(): Http ! Resource = group().map(of)

  /** an already-running group, for a caller who owns their own loops */
  def of(g: EventLoopGroup): Http = new Http:
    def send(r: Request): Response ! Async =
      Async.await[Response] { k =>
        val uri = URI.create(r.url)
        val port = if uri.getPort > 0 then uri.getPort
          else if uri.getScheme == "https" then 443 else 80
        val q = new okay.Channel[Chunk[Byte]](Int.MaxValue)
        val answered = java.util.concurrent.atomic.AtomicBoolean(false)

        val b = Bootstrap()
        b.group(g).channel(classOf[NioSocketChannel])
          .handler(new ChannelInitializer[SocketChannel] {
            def initChannel(ch: SocketChannel): Unit =
              ch.pipeline()
                .addLast(HttpClientCodec())
                // NO aggregator: the body is a Source, so content
                // arrives chunk by chunk and is told as it arrives
                .addLast(new SimpleChannelInboundHandler[HttpObject] {
                  def channelRead0(ctx: ChannelHandlerContext, msg: HttpObject): Unit =
                    msg match
                      case res: HttpResponse =>
                        val hs = res.headers.entries.asScala.toSeq
                          .map(e => (e.getKey, e.getValue))
                        if answered.compareAndSet(false, true) then
                          k(Right(Response(res.status.code, hs, Writer.of(q))))
                      case _ => ()
                    msg match
                      case c: HttpContent =>
                        val buf = c.content
                        if buf.readableBytes > 0 then
                          val a = new Array[Byte](buf.readableBytes)
                          buf.readBytes(a)
                          q.offer(scala.collection.immutable.ArraySeq.unsafeWrapArray(a)): Unit
                        if msg.isInstanceOf[LastHttpContent] then
                          q.close(); ctx.close(): Unit
                      case _ => ()

                  override def exceptionCaught(ctx: ChannelHandlerContext,
                                               e: Throwable): Unit =
                    if answered.compareAndSet(false, true) then k(Left(e))
                    else q.fail(e)
                    ctx.close(): Unit
                })
          })

        b.connect(uri.getHost, port).addListener { (f: ChannelFuture) =>
          if !f.isSuccess then
            if answered.compareAndSet(false, true) then k(Left(f.cause))
          else
            val path = if uri.getRawPath == null || uri.getRawPath.isEmpty then "/"
              else uri.getRawPath + (if uri.getRawQuery != null then "?" + uri.getRawQuery else "")
            val content = Unpooled.wrappedBuffer(r.body.bytes)
            val req = DefaultFullHttpRequest(HttpVersion.HTTP_1_1,
              HttpMethod.valueOf(r.method.name), path, content)
            req.headers.set(HttpHeaderNames.HOST, uri.getHost)
            req.headers.set(HttpHeaderNames.CONNECTION, HttpHeaderValues.CLOSE)
            req.headers.set(HttpHeaderNames.CONTENT_LENGTH, content.readableBytes)
            r.headers.foreach((h, v) => req.headers.set(h, v))
            f.channel.writeAndFlush(req): Unit
        }
        () => ()
      }

  // ---- the WebSocket client seam

  def sockets(): Sockets ! Resource = group().map { g =>
    new Sockets:
      def connect(url: String, headers: Seq[(String, String)],
                  subprotocols: Seq[String]): Socket ! Async =
        Async.await[Socket] { k =>
          val uri = URI.create(url)
          val port = if uri.getPort > 0 then uri.getPort
            else if uri.getScheme == "wss" then 443 else 80
          val q = new okay.Channel[Frame](Int.MaxValue)
          val opened = java.util.concurrent.atomic.AtomicBoolean(false)

          val hs = DefaultHttpHeaders()
          headers.foreach((h, v) => hs.set(h, v))
          val handshaker = WebSocketClientHandshakerFactory.newHandshaker(
            uri, WebSocketVersion.V13,
            if subprotocols.isEmpty then null else subprotocols.mkString(","),
            true, hs)

          val b = Bootstrap()
          b.group(g).channel(classOf[NioSocketChannel])
            .handler(new ChannelInitializer[SocketChannel] {
              def initChannel(ch: SocketChannel): Unit =
                ch.pipeline()
                  .addLast(HttpClientCodec())
                  .addLast(HttpObjectAggregator(65536))
                  .addLast(new SimpleChannelInboundHandler[Any] {
                    override def channelActive(ctx: ChannelHandlerContext): Unit =
                      handshaker.handshake(ctx.channel): Unit

                    def channelRead0(ctx: ChannelHandlerContext, msg: Any): Unit =
                      if !handshaker.isHandshakeComplete then
                        handshaker.finishHandshake(ctx.channel,
                          msg.asInstanceOf[FullHttpResponse])
                        if opened.compareAndSet(false, true) then
                          k(Right(socket(ctx.channel, q)))
                      else msg match
                        case f: WebSocketFrame => q.offer(frameOf(f)): Unit
                        case _ => ()

                    override def channelInactive(ctx: ChannelHandlerContext): Unit =
                      q.close()

                    override def exceptionCaught(ctx: ChannelHandlerContext,
                                                 e: Throwable): Unit =
                      if opened.compareAndSet(false, true) then k(Left(e))
                      else q.fail(e)
                      ctx.close(): Unit
                  })
            })
          b.connect(uri.getHost, port).addListener { (f: ChannelFuture) =>
            if !f.isSuccess && opened.compareAndSet(false, true) then k(Left(f.cause))
          }
          () => ()
        }
  }

  // ---- the server, both halves

  /**
   * REST routes and WebSocket sessions on one port, dispatched by the
   * same `Request` a client speaks — and a WebSocket session is the
   * same `Stage[Frame, Frame, Unit]` a client runs, as with Jetty.
   */
  def serve(port: Int)(routes: PartialFunction[Request, Response ! Async])
           (ws: PartialFunction[Request, okay.Stage[Frame, Frame, Unit]] =
              PartialFunction.empty)
           (using CanBlock, Scheduler): NettyChannel ! Resource =
    for
      boss <- group()
      work <- group()
      ch <- Resource.acquire {
        val b = ServerBootstrap()
        b.group(boss, work).channel(classOf[NioServerSocketChannel])
          .childHandler(new ChannelInitializer[SocketChannel] {
            def initChannel(c: SocketChannel): Unit =
              c.pipeline()
                .addLast(HttpServerCodec())
                .addLast(HttpObjectAggregator(1048576))
                .addLast(new SimpleChannelInboundHandler[FullHttpRequest] {
                  def channelRead0(ctx: ChannelHandlerContext,
                                   req: FullHttpRequest): Unit =
                    val r = requestOf(req)
                    if ws.isDefinedAt(r) &&
                      HttpHeaderValues.WEBSOCKET.contentEqualsIgnoreCase(
                        req.headers.get(HttpHeaderNames.UPGRADE)) then
                      upgrade(ctx, req, ws(r))
                    else answer(ctx, r, routes)
                })
          })
        b.bind(port).sync().channel
      }(c => { c.close().sync(): Unit })
    yield ch

  /** the port a server bound to — useful when 0 asked for any free one */
  def port(c: NettyChannel): Int =
    c.localAddress.asInstanceOf[java.net.InetSocketAddress].getPort

  private def requestOf(req: FullHttpRequest): Request =
    val hs = req.headers.entries.asScala.toSeq.map(e => (e.getKey, e.getValue))
    val m = Method.values.find(_.name == req.method.name).getOrElse(Method.Get)
    // the body travels too: an aggregated request HAS its content here,
    // and a route that cannot see it is a route that cannot POST
    val buf = req.content
    val body =
      if buf.readableBytes == 0 then Body.Empty
      else
        val a = new Array[Byte](buf.readableBytes)
        buf.getBytes(buf.readerIndex, a)
        Body.Bytes(scala.collection.immutable.ArraySeq.unsafeWrapArray(a))
    Request(m, req.uri, hs, body)

  private def answer(ctx: ChannelHandlerContext, r: Request,
                     routes: PartialFunction[Request, Response ! Async])
                    (using CanBlock): Unit =
    val (status, headers, bytes) =
      if !routes.isDefinedAt(r) then (404, Seq.empty[(String, String)], "not found".getBytes(UTF_8))
      else
        try
          val out = Async.run[Response, Pure](routes(r)).runWith
          (out.status, out.headers,
            Async.run[Chunk[Byte], Pure](Http.bytes(out)).runWith.toArray)
        catch
          // damage as data on the wire, as on every other server here
          case e: Throwable =>
            (500, Seq.empty[(String, String)],
              Option(e.getMessage).getOrElse(e.getClass.getName).getBytes(UTF_8))

    val res = DefaultFullHttpResponse(HttpVersion.HTTP_1_1,
      HttpResponseStatus.valueOf(status), Unpooled.wrappedBuffer(bytes))
    headers.foreach((k, v) => res.headers.set(k, v))
    res.headers.set(HttpHeaderNames.CONTENT_LENGTH, bytes.length)
    ctx.writeAndFlush(res).addListener(ChannelFutureListener.CLOSE): Unit

  private def upgrade(ctx: ChannelHandlerContext, req: FullHttpRequest,
                      stage: okay.Stage[Frame, Frame, Unit])
                     (using Scheduler): Unit =
    val q = new okay.Channel[Frame](Int.MaxValue)
    val handshaker = WebSocketServerHandshakerFactory(
      "ws://" + req.headers.get(HttpHeaderNames.HOST) + req.uri, null, true)
      .newHandshaker(req)
    if handshaker == null then
      WebSocketServerHandshakerFactory.sendUnsupportedVersionResponse(ctx.channel): Unit
    else
      ctx.pipeline.addLast(new SimpleChannelInboundHandler[WebSocketFrame] {
        def channelRead0(c: ChannelHandlerContext, f: WebSocketFrame): Unit =
          q.offer(frameOf(f)): Unit
        override def channelInactive(c: ChannelHandlerContext): Unit = q.close()
      })
      handshaker.handshake(ctx.channel, req).addListener { (f: ChannelFuture) =>
        if f.isSuccess then
          Async.spawn(okay.http.Ws.over(socket(ctx.channel, q))(stage)): Unit
      }

  // ---- frames, both directions

  private def frameOf(f: WebSocketFrame): Frame =
    def bytes(b: ByteBuf): Chunk[Byte] =
      val a = new Array[Byte](b.readableBytes); b.getBytes(b.readerIndex, a)
      scala.collection.immutable.ArraySeq.unsafeWrapArray(a)

    f match
      case t: TextWebSocketFrame => Frame.Text(t.text)
      case b: BinaryWebSocketFrame => Frame.Binary(bytes(b.content))
      case p: PingWebSocketFrame => Frame.Ping(bytes(p.content))
      case p: PongWebSocketFrame => Frame.Pong(bytes(p.content))
      case c: CloseWebSocketFrame =>
        Frame.Close(if c.statusCode == -1 then Frame.Normal else c.statusCode,
          Option(c.reasonText).getOrElse(""))
      case other => Frame.Binary(bytes(other.content))

  private def socket(ch: NettyChannel, q: okay.Channel[Frame]): Socket =
    new Socket:
      private val closed = java.util.concurrent.atomic.AtomicBoolean(false)

      def send(f: Frame): Unit ! Async = Async.await[Unit] { k =>
        val out: WebSocketFrame = f match
          case Frame.Text(t) => TextWebSocketFrame(t)
          case Frame.Binary(b) => BinaryWebSocketFrame(Unpooled.wrappedBuffer(b.toArray))
          case Frame.Ping(b) => PingWebSocketFrame(Unpooled.wrappedBuffer(b.toArray))
          case Frame.Pong(b) => PongWebSocketFrame(Unpooled.wrappedBuffer(b.toArray))
          case Frame.Close(c, r) => CloseWebSocketFrame(c, r)
        if f.isInstanceOf[Frame.Close] && !closed.compareAndSet(false, true) then
          k(Right(()))
        else
          ch.writeAndFlush(out).addListener { (fu: ChannelFuture) =>
            if fu.isSuccess then k(Right(())) else k(Left(fu.cause))
          }
        () => ()
      }

      def frames: Source[Frame] = Writer.of(q)

      def close(code: Int, reason: String): Unit ! Async =
        send(Frame.Close(code, reason))
}
