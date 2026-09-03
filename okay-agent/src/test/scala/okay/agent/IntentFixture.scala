package okay.agent

import okay.codec.Schema

/**
 * The labelled fixture the intent lanes measure against
 * (specs/intent-classify.md).
 *
 * It lives in a source rather than a resource file so both platforms
 * can read it, and it is shared rather than private to one suite so
 * the next lane compares against the SAME messages instead of
 * inventing its own and quietly moving the baseline.
 *
 * THE DOMAIN, stated because the previous lane discovered that nothing
 * else states it: meeting and scheduling intents. `Proposal` proposes a
 * time or a meeting, `Request` asks for an action or an artefact around
 * one, `Notification` informs with nothing to do, and `Other` is a
 * message that is not about arranging anything at all. That last
 * boundary is exactly where the classifier lost the bucket, so it is
 * where the hard cases were put on purpose rather than avoided.
 *
 * The taxonomy here is deliberately FLAT and its cases carry one plain
 * slot. The question these messages settle is whether the
 * out-of-domain bucket survives, and a hierarchy or a slot that can
 * itself fail (an ISO-8601 `When`) would add failure modes that
 * confound that answer. `TestClassify` keeps the nested taxonomy for
 * the structural claims.
 *
 * HONEST LIMITATION: these messages are written by the author of the
 * classifier, not sampled from real traffic. They can only show that a
 * change moves the needle on cases someone thought of; they cannot
 * show coverage. Thirty per class is the reference's minimum for
 * stable per-class metrics, not a claim of representativeness.
 */
object IntentFixture {

  enum Support derives Schema:
    case Proposal(what: String)
    case Request(what: String)
    case Notification(what: String)
    case Other(what: String)

  /**
   * The SAME four classes, named so that the names say what the
   * subject is.
   *
   * This exists to test one thing: whether a taxonomy carries its
   * domain in its case names or nowhere. `Support` above says
   * `Proposal`/`Request`/`Notification` and carries a bare
   * `what: String`, which never mentions meetings — so "please refund
   * my card" reads as a `Request` honestly rather than mistakenly, and
   * every prompt-level fix for that is arguing with a type that did
   * not state its subject.
   */
  enum Meeting derives Schema:
    case MeetingProposal(what: String)
    case MeetingRequest(what: String)
    case MeetingNotification(what: String)
    case NotAboutMeetings(what: String)

  /** the domain-bearing names, mapped back to the canonical classes,
   * so two taxonomies are scored on ONE axis */
  val canonical: Map[String, String] = Map(
    "MeetingProposal" -> "Proposal",
    "MeetingRequest" -> "Request",
    "MeetingNotification" -> "Notification",
    "NotAboutMeetings" -> "Other")

  /** proposing or moving a time */
  val proposals: List[(String, String)] = List(
    "Putting forward a meeting next Thursday at 2pm." -> "Proposal",
    "Can we move Thursday's sync to Friday morning instead?" -> "Proposal",  // reads as a request too
    "How about we meet next Tuesday to go over the numbers?" -> "Proposal",
    "I'd like to suggest a 30-minute call sometime this week." -> "Proposal",
    "Shall we reschedule our 1:1 to after the release?" -> "Proposal",
    "Proposing we push the design review to next Monday 10am." -> "Proposal",
    "Would Wednesday at 3 work for a quick chat?" -> "Proposal",
    "Let's grab 15 minutes before standup tomorrow." -> "Proposal",  // imperative, still a proposal
    "I was thinking we could do the retro on Thursday for once." -> "Proposal",  // indirect
    "What if we split this into two sessions, one per team?" -> "Proposal",  // indirect
    "Happy to walk you through it live if that is easier - say Friday?" -> "Proposal",  // hedged
    "Any chance of moving our call an hour later?" -> "Proposal",  // hedged, overlaps Request
    "Suggestion: we meet on Monday at 9." -> "Proposal",
    "Suggest we cancel Monday and meet Wednesday instead." -> "Proposal",  // cancel + propose
    "Perhaps a short sync before the board meeting would help." -> "Proposal",  // very indirect
    "I can do Tuesday or Thursday afternoon, whichever suits you." -> "Proposal",  // offering slots
    "Should we bring the customer call forward to this week?" -> "Proposal",
    "Fancy a coffee chat about the roadmap on Friday?" -> "Proposal",  // informal
    "Available to meet on Tuesday afternoon." -> "Proposal",
    "Let me know if a 45-minute deep dive next week makes sense." -> "Proposal",  // hedged
    "We could combine the two reviews into one hour on Monday." -> "Proposal",
    "Thinking of a kickoff on the 12th - does that clash for you?" -> "Proposal",
    "How does next Wednesday look for the quarterly planning?" -> "Proposal",
    "Open to meeting earlier if that helps you catch your train." -> "Proposal",
    "Instead of email, shall we just talk it through on Thursday?" -> "Proposal",
    "I propose we hold the postmortem once the incident is closed." -> "Proposal",
    "Would it help to add a weekly 20-minute check-in?" -> "Proposal",  // proposes a recurring meeting
    "Can we push everything back a week given the holidays?" -> "Proposal",
    "Might be worth a joint session with design next sprint." -> "Proposal",  // very indirect
    "Free any afternoon this week if you want to go over it." -> "Proposal")  // offer without a verb

  /** asking for an action or an artefact around a meeting */
  val requests: List[(String, String)] = List(
    "Could you send me the deck before tomorrow's meeting?" -> "Request",
    "Please confirm whether you can attend on Friday." -> "Request",
    "Can you share the notes from yesterday's standup?" -> "Request",
    "I need the invoice for last month, could you forward it?" -> "Request",
    "Would you be able to review my PR before the demo?" -> "Request",
    "Please book a room for six people for Wednesday." -> "Request",
    "Send me the agenda when you get a chance." -> "Request",
    "Can someone add me to the invite for the sync?" -> "Request",
    "Please forward the recording to the people who missed it." -> "Request",
    "Could you check whether the room has a working camera?" -> "Request",
    "I would appreciate the slides in advance this time." -> "Request",  // indirect
    "Any chance you could take the minutes on Thursday?" -> "Request",  // hedged, overlaps Proposal
    "Kindly send me the agenda." -> "Request",
    "Please decline the meeting on my behalf if I am not needed." -> "Request",
    "Can you set up the call with their procurement team?" -> "Request",
    "Could you dial in five minutes early to test the audio?" -> "Request",
    "Please update the invite - the link is the old one." -> "Request",
    "Would you mind chairing the review while I am away?" -> "Request",
    "Send over the questions you want covered before we meet." -> "Request",
    "Can you get me the attendee list by end of day?" -> "Request",
    "Please add the finance team to Thursday's invite." -> "Request",
    "Could you print the handouts for the workshop?" -> "Request",
    "Please send the link to the meeting." -> "Request",
    "I need someone to cover the demo slot on Friday." -> "Request",  // no explicit addressee
    "Please move the recurring invite off Monday mornings." -> "Request",  // overlaps Proposal
    "Can you confirm the room booking went through?" -> "Request",
    "Would you share the budget numbers ahead of the call?" -> "Request",
    "Please let me know who else should be in the room." -> "Request",
    "Could you follow up with them about a date?" -> "Request",  // asks someone else to propose
    "Send a calendar hold while we work out the details." -> "Request")

  /** informing, with nothing to do */
  val notifications: List[(String, String)] = List(
    "Just letting you know the office will be closed on Monday." -> "Notification",
    "FYI the meeting room has been changed to B2." -> "Notification",
    "Heads up: I will be on leave next week." -> "Notification",
    "The quarterly report has been published on the intranet." -> "Notification",
    "Reminder that the deadline is this Friday." -> "Notification",
    "Our call tomorrow is cancelled, no action needed." -> "Notification",  // cancellation without a proposal
    "The recording of yesterday's session is now available." -> "Notification",
    "I will be dialling in from the airport, audio only." -> "Notification",
    "Note that the agenda has been updated since I sent it." -> "Notification",
    "The workshop is full; no further sign-ups are possible." -> "Notification",
    "Please note the building requires a badge after 7pm." -> "Notification",  // imperative, still informational
    "Our guest speaker has confirmed for the November session." -> "Notification",
    "The session is going ahead as planned." -> "Notification",
    "The invite went out with the wrong time zone; it is fixed now." -> "Notification",
    "I have accepted the invitation for Thursday." -> "Notification",
    "The project channel has moved, links in the old one still work." -> "Notification",
    "We reached quorum, so the vote stands." -> "Notification",
    "Minutes from the last meeting are attached." -> "Notification",  // attachment, no ask
    "The meeting has moved to the other building." -> "Notification",
    "Catering will be provided, so no need to bring lunch." -> "Notification",
    "The room is double-booked but we have the priority claim." -> "Notification",
    "For transparency: the decision was taken without a meeting." -> "Notification",
    "I am no longer the owner of this recurring session." -> "Notification",
    "The all-hands ran long, the Q and A was cut short." -> "Notification",
    "Our external guest cannot join, we will proceed as planned." -> "Notification",
    "The link in the invite now points to the new platform." -> "Notification",
    "This is the last reminder before the series ends." -> "Notification",
    "The office move means Thursdays are remote from now on." -> "Notification",
    "Attendance was noted, no follow-up is expected from you." -> "Notification",
    "I have summarised the discussion in the shared doc." -> "Notification")

  /** not about arranging anything - where the bucket collapsed */
  val others: List[(String, String)] = List(
    "My card was charged twice this month, please refund." -> "Other",  // HARD: a Request in register, out of domain
    "The app crashes every time I open the billing page." -> "Other",  // HARD: support issue
    "Happy birthday! Hope you have a great day." -> "Other",
    "Thanks a lot, that was really helpful." -> "Other",
    "I want to cancel my subscription effective immediately." -> "Other",  // HARD: a Request in register
    "Here is the recipe you asked about at lunch." -> "Other",  // HARD: looks like a Notification
    "What is the capital of Portugal?" -> "Other",  // general knowledge
    "Congratulations on the promotion, well deserved!" -> "Other",
    "My password reset link has expired, can you send another?" -> "Other",  // HARD: a Request in register
    "The parcel arrived damaged and I would like a replacement." -> "Other",  // HARD: a Request in register
    "Good morning! Coffee before we start?" -> "Other",  // HARD: brushes against a Proposal
    "I loved the book you recommended." -> "Other",
    "Can you recommend a good dentist nearby?" -> "Other",  // HARD: a Request, wrong domain
    "Our cat had kittens over the weekend." -> "Other",
    "Please stop sending me marketing emails." -> "Other",  // HARD: a Request in register
    "Do you know if the canteen is open on Sundays?" -> "Other",  // HARD: a question, wrong domain
    "Just finished the marathon, absolutely wrecked." -> "Other",
    "The invoice number on my statement does not match yours." -> "Other",  // HARD: sounds administrative
    "Wishing you a restful holiday break." -> "Other",
    "I am writing to complain about the noise from the works." -> "Other",  // HARD: a complaint
    "Attached is the poem I mentioned." -> "Other",  // HARD: looks like a Notification
    "Any idea why my laptop keeps disconnecting from wifi?" -> "Other",  // HARD: a Request, wrong domain
    "Great match yesterday, did you watch it?" -> "Other",
    "Please update my home address in your records." -> "Other",  // HARD: a Request in register
    "The weather forecast says snow on the weekend." -> "Other",  // HARD: looks like a Notification
    "I have accepted the job offer, starting in March." -> "Other",  // HARD: personal news
    "Could you water my plants while I am away?" -> "Other",  // HARD: a Request, wrong domain
    "This newsletter is excellent, keep it up." -> "Other",
    "My order still has not been delivered." -> "Other",  // HARD: a complaint
    "Reminder: renew your gym membership this month." -> "Other")  // HARD: a Notification, wrong domain

  /** message, and the case name it should be read as */
  val labelled: List[(String, String)] =
    proposals ++ requests ++ notifications ++ others

  /** examples for the prompt - deliberately NOT drawn from `labelled`,
   * so an arm that shows examples is not being scored on its own
   * teaching material */
  val examples: List[(String, Support)] = List(
    "Are you free to meet on Wednesday afternoon?" -> Support.Proposal("meet Wednesday"),
    "Please forward me the signed contract." -> Support.Request("forward the contract"),
    "Note that payroll runs a day early this month." -> Support.Notification("payroll early"),
    "What is the capital of Portugal?" -> Support.Other("general knowledge"),
    "My headphones arrived broken, I want a replacement." -> Support.Other("a support issue"))

  /** the same five examples in the domain-bearing taxonomy, so the
   * arms differ in the NAMES and in nothing else */
  val meetingExamples: List[(String, Meeting)] = List(
    "Are you free to meet on Wednesday afternoon?" -> Meeting.MeetingProposal("meet Wednesday"),
    "Please forward me the signed contract." -> Meeting.MeetingRequest("forward the contract"),
    "Note that payroll runs a day early this month." -> Meeting.MeetingNotification("payroll early"),
    "What is the capital of Portugal?" -> Meeting.NotAboutMeetings("general knowledge"),
    "My headphones arrived broken, I want a replacement." -> Meeting.NotAboutMeetings("a support issue"))


  // ----------------------------------------------------------------
  // The same intents, in several languages.
  //
  // Scattering a few foreign sentences through the lists above proved
  // nothing: a miss could always be the sentence rather than the
  // language. Here each message is ONE meaning in six languages, so
  // "does this hold outside English" is a per-language number and a
  // drop is attributable. Same author-written limitation as the rest,
  // and one more besides: these are translations by the same hand that
  // wrote the classifier, so an awkward rendering is a confound the
  // numbers cannot separate from a model weakness.

  /** one meaning, its class, and its wording per language */
  final case class Parallel(id: String, label: String, byLang: Map[String, String])

  val languages: List[String] = List("en", "fr", "de", "es", "ru", "ja")

  val parallel: List[Parallel] = List(
    Parallel("meet-tuesday", "Proposal", Map(
      "en" -> "Can we meet on Tuesday at 3pm?",
      "fr" -> "Pouvons-nous nous voir mardi à 15h ?",
      "de" -> "Können wir uns am Dienstag um 15 Uhr treffen?",
      "es" -> "¿Podemos reunirnos el martes a las 15:00?",
      "ru" -> "Можем встретиться во вторник в 15:00?",
      "ja" -> "火曜日の15時に会えますか？")),
    Parallel("move-review", "Proposal", Map(
      "en" -> "I suggest we move the review to next week.",
      "fr" -> "Je propose de reporter la revue à la semaine prochaine.",
      "de" -> "Ich schlage vor, das Review auf nächste Woche zu verschieben.",
      "es" -> "Propongo aplazar la revisión a la próxima semana.",
      "ru" -> "Предлагаю перенести ревью на следующую неделю.",
      "ja" -> "レビューを来週に延期することを提案します。")),
    Parallel("friday-call", "Proposal", Map(
      "en" -> "Would Friday morning work for a short call?",
      "fr" -> "Vendredi matin conviendrait-il pour un court appel ?",
      "de" -> "Würde Freitagvormittag für ein kurzes Gespräch passen?",
      "es" -> "¿Le vendría bien el viernes por la mañana para una llamada corta?",
      "ru" -> "Подойдёт ли пятница утром для короткого звонка?",
      "ja" -> "金曜日の午前中に短い通話はいかがでしょうか。")),
    Parallel("send-agenda", "Request", Map(
      "en" -> "Please send me the agenda before the meeting.",
      "fr" -> "Merci de m'envoyer l'ordre du jour avant la réunion.",
      "de" -> "Bitte senden Sie mir die Tagesordnung vor der Sitzung.",
      "es" -> "Por favor, envíeme el orden del día antes de la reunión.",
      "ru" -> "Пожалуйста, пришлите мне повестку до встречи.",
      "ja" -> "会議の前に議題を送ってください。")),
    Parallel("book-room", "Request", Map(
      "en" -> "Could you book a room for four people?",
      "fr" -> "Pourriez-vous réserver une salle pour quatre personnes ?",
      "de" -> "Könnten Sie einen Raum für vier Personen buchen?",
      "es" -> "¿Podría reservar una sala para cuatro personas?",
      "ru" -> "Не могли бы вы забронировать комнату на четверых?",
      "ja" -> "4人用の会議室を予約していただけますか。")),
    Parallel("confirm-attend", "Request", Map(
      "en" -> "Please confirm whether you can attend.",
      "fr" -> "Merci de confirmer si vous pouvez participer.",
      "de" -> "Bitte bestätigen Sie, ob Sie teilnehmen können.",
      "es" -> "Por favor, confirme si puede asistir.",
      "ru" -> "Пожалуйста, подтвердите, сможете ли вы присутствовать.",
      "ja" -> "ご出席いただけるかご確認ください。")),
    Parallel("room-changed", "Notification", Map(
      "en" -> "The meeting room has changed to B2.",
      "fr" -> "La salle de réunion a été changée en B2.",
      "de" -> "Der Besprechungsraum wurde auf B2 geändert.",
      "es" -> "La sala de reuniones ha cambiado a B2.",
      "ru" -> "Переговорная изменена на B2.",
      "ja" -> "会議室がB2に変更されました。")),
    Parallel("on-leave", "Notification", Map(
      "en" -> "I will be on leave next week.",
      "fr" -> "Je serai en congé la semaine prochaine.",
      "de" -> "Ich bin nächste Woche im Urlaub.",
      "es" -> "Estaré de vacaciones la próxima semana.",
      "ru" -> "На следующей неделе я буду в отпуске.",
      "ja" -> "来週は休暇を取ります。")),
    Parallel("call-cancelled", "Notification", Map(
      "en" -> "Tomorrow's call is cancelled, nothing to do.",
      "fr" -> "L'appel de demain est annulé, rien à faire.",
      "de" -> "Der morgige Anruf ist abgesagt, es ist nichts zu tun.",
      "es" -> "La llamada de mañana está cancelada, no hay nada que hacer.",
      "ru" -> "Завтрашний звонок отменён, ничего делать не нужно.",
      "ja" -> "明日の通話は中止です。対応は不要です。")),
    Parallel("birthday", "Other", Map(
      "en" -> "Happy birthday! Have a great day.",
      "fr" -> "Joyeux anniversaire ! Passe une excellente journée.",
      "de" -> "Alles Gute zum Geburtstag! Hab einen schönen Tag.",
      "es" -> "¡Feliz cumpleaños! Que tengas un gran día.",
      "ru" -> "С днём рождения! Отличного дня.",
      "ja" -> "お誕生日おめでとうございます。素敵な一日を。")),
    Parallel("charged-twice", "Other", Map(   // HARD: a Request in register
      "en" -> "My card was charged twice, please refund.",
      "fr" -> "Ma carte a été débitée deux fois, merci de me rembourser.",
      "de" -> "Meine Karte wurde zweimal belastet, bitte erstatten Sie den Betrag.",
      "es" -> "Me cobraron dos veces en la tarjeta, por favor devuélvanme el dinero.",
      "ru" -> "С моей карты списали дважды, прошу вернуть деньги.",
      "ja" -> "カードから二重に請求されました。返金をお願いします。")),
    Parallel("app-crashes", "Other", Map(   // HARD: a support issue
      "en" -> "The app crashes when I open the billing page.",
      "fr" -> "L'application plante quand j'ouvre la page de facturation.",
      "de" -> "Die App stürzt ab, wenn ich die Rechnungsseite öffne.",
      "es" -> "La aplicación se cierra cuando abro la página de facturación.",
      "ru" -> "Приложение падает, когда я открываю страницу оплаты.",
      "ja" -> "請求ページを開くとアプリが落ちます。")))

  /** the parallel set as (message, class) pairs for one language */
  def inLanguage(lang: String): List[(String, String)] =
    parallel.flatMap(p => p.byLang.get(lang).map(_ -> p.label))

  val classes: List[String] = List("Proposal", "Request", "Notification", "Other")
}
