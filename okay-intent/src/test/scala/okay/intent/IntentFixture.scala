package okay.intent

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

  /**
   * A WRONG domain: the same four classes named for a subject these
   * messages are not about.
   *
   * If the model reads the domain word rather than treating a
   * qualifier as decoration, meeting messages should land in
   * `NotAboutShipping` and the positive classes should empty out. That
   * would be a FAILURE of accuracy and a SUCCESS of the hypothesis,
   * which is exactly why the arm is worth running.
   */
  enum Shipping derives Schema:
    case ShippingProposal(what: String)
    case ShippingRequest(what: String)
    case ShippingNotification(what: String)
    case NotAboutShipping(what: String)

  /**
   * A qualifier that names nothing at all.
   *
   * The control the previous lane was missing: if `Zarnic` lifts
   * `Other` the way `Meeting` did, then what helped was that the names
   * look deliberately chosen, not the domain they name — and the
   * recommendation that shipped is weaker than it sounds.
   */
  enum Zarnic derives Schema:
    case ZarnicProposal(what: String)
    case ZarnicRequest(what: String)
    case ZarnicNotification(what: String)
    case NotAboutZarnic(what: String)

  /**
   * The SAME four classes, named in each language of the parallel set.
   *
   * The first candidate for the language gap: a domain-bearing name is
   * what rescued `Other` in English, and every measurement since has
   * been made with ENGLISH names read against non-English messages. If
   * the name has to be understood for the domain to land, a reader who
   * is working in Russian is being handed the domain in a second
   * language.
   *
   * Scala identifiers may be non-ASCII, so this costs nothing but the
   * typing — which is the point of testing it before inventing
   * machinery.
   */
  enum RencontreFr derives Schema:
    case PropositionDeReunion(quoi: String)
    case DemandeDeReunion(quoi: String)
    case InformationDeReunion(quoi: String)
    case PasUneReunion(quoi: String)

  enum BesprechungDe derives Schema:
    case Besprechungsvorschlag(was: String)
    case Besprechungsanfrage(was: String)
    case Besprechungshinweis(was: String)
    case KeineBesprechung(was: String)

  enum ReunionEs derives Schema:
    case PropuestaDeReunion(que: String)
    case SolicitudDeReunion(que: String)
    case AvisoDeReunion(que: String)
    case NoEsUnaReunion(que: String)

  enum ВстречаRu derives Schema:
    case ПредложениеВстречи(что: String)
    case ПросьбаПоВстрече(что: String)
    case СообщениеОВстрече(что: String)
    case НеПроВстречу(что: String)

  enum 会議Ja derives Schema:
    case 会議の提案(内容: String)
    case 会議の依頼(内容: String)
    case 会議の連絡(内容: String)
    case 会議ではない(内容: String)

  /**
   * The same five examples for each language taxonomy.
   *
   * The example MESSAGES stay English on purpose: this experiment
   * isolates the NAMES, and translating the messages too would move
   * two things at once. The residual oddity — Russian class names
   * beside English examples — is the price of isolating a variable,
   * and it is stated rather than hidden.
   */
  val examplesFr: List[(String, RencontreFr)] = List(
    "Are you free to meet on Wednesday afternoon?" -> RencontreFr.PropositionDeReunion("meet Wednesday"),
    "Please forward me the signed contract." -> RencontreFr.DemandeDeReunion("forward the contract"),
    "Note that payroll runs a day early this month." -> RencontreFr.InformationDeReunion("payroll early"),
    "What is the capital of Portugal?" -> RencontreFr.PasUneReunion("general knowledge"),
    "My headphones arrived broken, I want a replacement." -> RencontreFr.PasUneReunion("a support issue"))

  val examplesDe: List[(String, BesprechungDe)] = List(
    "Are you free to meet on Wednesday afternoon?" -> BesprechungDe.Besprechungsvorschlag("meet Wednesday"),
    "Please forward me the signed contract." -> BesprechungDe.Besprechungsanfrage("forward the contract"),
    "Note that payroll runs a day early this month." -> BesprechungDe.Besprechungshinweis("payroll early"),
    "What is the capital of Portugal?" -> BesprechungDe.KeineBesprechung("general knowledge"),
    "My headphones arrived broken, I want a replacement." -> BesprechungDe.KeineBesprechung("a support issue"))

  val examplesEs: List[(String, ReunionEs)] = List(
    "Are you free to meet on Wednesday afternoon?" -> ReunionEs.PropuestaDeReunion("meet Wednesday"),
    "Please forward me the signed contract." -> ReunionEs.SolicitudDeReunion("forward the contract"),
    "Note that payroll runs a day early this month." -> ReunionEs.AvisoDeReunion("payroll early"),
    "What is the capital of Portugal?" -> ReunionEs.NoEsUnaReunion("general knowledge"),
    "My headphones arrived broken, I want a replacement." -> ReunionEs.NoEsUnaReunion("a support issue"))

  val examplesRu: List[(String, ВстречаRu)] = List(
    "Are you free to meet on Wednesday afternoon?" -> ВстречаRu.ПредложениеВстречи("meet Wednesday"),
    "Please forward me the signed contract." -> ВстречаRu.ПросьбаПоВстрече("forward the contract"),
    "Note that payroll runs a day early this month." -> ВстречаRu.СообщениеОВстрече("payroll early"),
    "What is the capital of Portugal?" -> ВстречаRu.НеПроВстречу("general knowledge"),
    "My headphones arrived broken, I want a replacement." -> ВстречаRu.НеПроВстречу("a support issue"))

  val examplesJa: List[(String, 会議Ja)] = List(
    "Are you free to meet on Wednesday afternoon?" -> 会議Ja.会議の提案("meet Wednesday"),
    "Please forward me the signed contract." -> 会議Ja.会議の依頼("forward the contract"),
    "Note that payroll runs a day early this month." -> 会議Ja.会議の連絡("payroll early"),
    "What is the capital of Portugal?" -> 会議Ja.会議ではない("general knowledge"),
    "My headphones arrived broken, I want a replacement." -> 会議Ja.会議ではない("a support issue"))

  /** the second candidate: say the subject out loud, in the reader's
   * language, and leave the (English) names alone */
  val domainSentence: Map[String, String] = Map(
    "en" -> "The subject matter is meetings and scheduling.",
    "fr" -> "Le sujet est les réunions et la planification.",
    "de" -> "Das Thema sind Besprechungen und Terminplanung.",
    "es" -> "El tema son las reuniones y la planificación.",
    "ru" -> "Тема — встречи и планирование.",
    "ja" -> "話題は会議と日程調整です。")

  /** every taxonomy's names mapped back to the canonical classes, so
   * they are all scored on ONE axis */
  val canonical: Map[String, String] = Map(
    "MeetingProposal" -> "Proposal",
    "MeetingRequest" -> "Request",
    "MeetingNotification" -> "Notification",
    "NotAboutMeetings" -> "Other",
    "ShippingProposal" -> "Proposal",
    "ShippingRequest" -> "Request",
    "ShippingNotification" -> "Notification",
    "NotAboutShipping" -> "Other",
    "ZarnicProposal" -> "Proposal",
    "ZarnicRequest" -> "Request",
    "ZarnicNotification" -> "Notification",
    "NotAboutZarnic" -> "Other",
    "PropositionDeReunion" -> "Proposal",
    "DemandeDeReunion" -> "Request",
    "InformationDeReunion" -> "Notification",
    "PasUneReunion" -> "Other",
    "Besprechungsvorschlag" -> "Proposal",
    "Besprechungsanfrage" -> "Request",
    "Besprechungshinweis" -> "Notification",
    "KeineBesprechung" -> "Other",
    "PropuestaDeReunion" -> "Proposal",
    "SolicitudDeReunion" -> "Request",
    "AvisoDeReunion" -> "Notification",
    "NoEsUnaReunion" -> "Other",
    "ПредложениеВстречи" -> "Proposal",
    "ПросьбаПоВстрече" -> "Request",
    "СообщениеОВстрече" -> "Notification",
    "НеПроВстречу" -> "Other",
    "会議の提案" -> "Proposal",
    "会議の依頼" -> "Request",
    "会議の連絡" -> "Notification",
    "会議ではない" -> "Other")

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

  /**
   * Two examples that settle a tie, instead of two sentences that
   * state it.
   *
   * The precedence lane put the same two rules into the prompt as
   * prose and lost 0.043 macro F1, diluting every class including the
   * one the rule named. These carry the identical decisions —
   * proposal-beats-request when a message does both, and a
   * cancellation with no new time is a notification — as instances of
   * the disputed case, which is the channel that has paid every time
   * it was tried in this line.
   *
   * Deliberately not drawn from `labelled`, like the others.
   */
  val tieBreakExamples: List[(String, Meeting)] = List(
    "Could you move our Tuesday call to Thursday instead?" ->
      Meeting.MeetingProposal("move the call to Thursday"),
    "I am cancelling Friday's review; nothing to reschedule for now." ->
      Meeting.MeetingNotification("Friday's review cancelled"))

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
      "ru" -> "Созвонимся ненадолго в пятницу утром?",
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
      "ru" -> "Забронируйте, пожалуйста, комнату на четверых.",
      "ja" -> "4人用の会議室を予約していただけますか。")),
    Parallel("confirm-attend", "Request", Map(
      "en" -> "Please confirm whether you can attend.",
      "fr" -> "Merci de confirmer si vous pouvez participer.",
      "de" -> "Bitte bestätigen Sie, ob Sie teilnehmen können.",
      "es" -> "Por favor, confirme si puede asistir.",
      "ru" -> "Подтвердите участие, пожалуйста.",
      "ja" -> "ご出席いただけるかご確認ください。")),
    Parallel("room-changed", "Notification", Map(
      "en" -> "The meeting room has changed to B2.",
      "fr" -> "La salle de réunion a été changée en B2.",
      "de" -> "Der Besprechungsraum wurde auf B2 geändert.",
      "es" -> "La sala de reuniones ha cambiado a B2.",
      "ru" -> "Встреча перенесена в переговорную B2.",
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
      "ja" -> "請求ページを開くとアプリが落ちます。")),
    Parallel("propose-earlier", "Proposal", Map(
      "en" -> "Could we start the meeting an hour earlier?",
      "fr" -> "Pourrions-nous commencer la réunion une heure plus tôt ?",
      "de" -> "Könnten wir die Besprechung eine Stunde früher beginnen?",
      "es" -> "¿Podríamos empezar la reunión una hora antes?",
      "ru" -> "Давайте начнём встречу на час раньше.",
      "ja" -> "会議を1時間早く始められますか。")),
    Parallel("propose-split", "Proposal", Map(
      "en" -> "Let's split this into two shorter sessions.",
      "fr" -> "Séparons cela en deux séances plus courtes.",
      "de" -> "Teilen wir das in zwei kürzere Sitzungen auf.",
      "es" -> "Dividamos esto en dos sesiones más cortas.",
      "ru" -> "Давайте разделим это на две более короткие встречи.",
      "ja" -> "これを2回の短いセッションに分けましょう。")),
    Parallel("propose-weekly", "Proposal", Map(
      "en" -> "Shall we add a weekly check-in on Mondays?",
      "fr" -> "Ajoutons-nous un point hebdomadaire le lundi ?",
      "de" -> "Sollen wir einen wöchentlichen Termin am Montag einrichten?",
      "es" -> "¿Añadimos una reunión semanal los lunes?",
      "ru" -> "Добавим еженедельную встречу по понедельникам?",
      "ja" -> "毎週月曜日に定例を追加しませんか。")),
    Parallel("propose-after", "Proposal", Map(
      "en" -> "How about we meet after the release instead?",
      "fr" -> "Et si nous nous voyions plutôt après la mise en production ?",
      "de" -> "Wie wäre es, wenn wir uns stattdessen nach dem Release treffen?",
      "es" -> "¿Qué tal si nos vemos después del lanzamiento?",
      "ru" -> "Может, встретимся после релиза?",
      "ja" -> "リリース後に会うのはいかがでしょうか。")),
    Parallel("request-slides", "Request", Map(
      "en" -> "Please send the slides before Thursday.",
      "fr" -> "Merci d'envoyer les diapositives avant jeudi.",
      "de" -> "Bitte senden Sie die Folien vor Donnerstag.",
      "es" -> "Por favor, envíe las diapositivas antes del jueves.",
      "ru" -> "Слайды нужны мне к четвергу.",
      "ja" -> "木曜日までにスライドを送ってください。")),
    Parallel("request-invite", "Request", Map(
      "en" -> "Could you add me to the invitation?",
      "fr" -> "Pourriez-vous m'ajouter à l'invitation ?",
      "de" -> "Könnten Sie mich zur Einladung hinzufügen?",
      "es" -> "¿Podría añadirme a la invitación?",
      "ru" -> "Добавьте меня, пожалуйста, в приглашение.",
      "ja" -> "招待に私を追加していただけますか。")),
    Parallel("request-notes", "Request", Map(
      "en" -> "Please take the minutes this time.",
      "fr" -> "Merci de prendre les notes cette fois.",
      "de" -> "Bitte führen Sie diesmal das Protokoll.",
      "es" -> "Por favor, tome el acta esta vez.",
      "ru" -> "В этот раз протокол за вами.",
      "ja" -> "今回は議事録をお願いします。")),
    Parallel("request-room", "Request", Map(
      "en" -> "Can you check whether the room is free?",
      "fr" -> "Pouvez-vous vérifier si la salle est libre ?",
      "de" -> "Können Sie prüfen, ob der Raum frei ist?",
      "es" -> "¿Puede comprobar si la sala está libre?",
      "ru" -> "Проверьте, пожалуйста, свободна ли переговорная.",
      "ja" -> "会議室が空いているか確認できますか。")),
    Parallel("notify-late", "Notification", Map(
      "en" -> "I will be ten minutes late.",
      "fr" -> "Je serai en retard de dix minutes.",
      "de" -> "Ich komme zehn Minuten später.",
      "es" -> "Llegaré diez minutos tarde.",
      "ru" -> "Я опоздаю на десять минут.",
      "ja" -> "10分ほど遅れます。")),
    Parallel("notify-agenda", "Notification", Map(
      "en" -> "The agenda has been updated.",
      "fr" -> "L'ordre du jour a été mis à jour.",
      "de" -> "Die Tagesordnung wurde aktualisiert.",
      "es" -> "El orden del día se ha actualizado.",
      "ru" -> "Повестка обновлена.",
      "ja" -> "議題が更新されました。")),
    Parallel("notify-remote", "Notification", Map(
      "en" -> "Thursdays are remote from now on.",
      "fr" -> "Les jeudis se feront à distance désormais.",
      "de" -> "Donnerstags wird ab jetzt remote gearbeitet.",
      "es" -> "A partir de ahora los jueves son en remoto.",
      "ru" -> "С этого месяца по четвергам работаем удалённо.",
      "ja" -> "今後、木曜日はリモートになります。")),
    Parallel("notify-recording", "Notification", Map(
      "en" -> "Yesterday's recording is available now.",
      "fr" -> "L'enregistrement d'hier est maintenant disponible.",
      "de" -> "Die Aufzeichnung von gestern ist jetzt verfügbar.",
      "es" -> "La grabación de ayer ya está disponible.",
      "ru" -> "Вчерашняя запись уже доступна.",
      "ja" -> "昨日の録画が利用可能になりました。")),
    Parallel("other-thanks", "Other", Map(
      "en" -> "Thank you, that was really helpful.",
      "fr" -> "Merci, cela m'a beaucoup aidé.",
      "de" -> "Danke, das war sehr hilfreich.",
      "es" -> "Gracias, fue de mucha ayuda.",
      "ru" -> "Спасибо, это очень помогло.",
      "ja" -> "ありがとうございます。とても助かりました。")),
    Parallel("other-recipe", "Other", Map(
      "en" -> "Here is the recipe you asked about at lunch.",
      "fr" -> "Voici la recette dont tu m'as parlé au déjeuner.",
      "de" -> "Hier ist das Rezept, nach dem du beim Mittagessen gefragt hast.",
      "es" -> "Aquí está la receta que me pediste en la comida.",
      "ru" -> "Вот рецепт, о котором ты спрашивал за обедом.",
      "ja" -> "昼食のときに聞かれたレシピです。")),
    Parallel("other-weather", "Other", Map(
      "en" -> "It is supposed to snow this weekend.",
      "fr" -> "Il doit neiger ce week-end.",
      "de" -> "Am Wochenende soll es schneien.",
      "es" -> "Se espera que nieve este fin de semana.",
      "ru" -> "На выходных обещают снег.",
      "ja" -> "今週末は雪が降るそうです。")),
    Parallel("other-password", "Other", Map(
      "en" -> "My password reset link has expired.",
      "fr" -> "Mon lien de réinitialisation a expiré.",
      "de" -> "Mein Link zum Zurücksetzen des Passworts ist abgelaufen.",
      "es" -> "Mi enlace para restablecer la contraseña ha caducado.",
      "ru" -> "Ссылка для сброса пароля истекла.",
      "ja" -> "パスワード再設定のリンクの有効期限が切れました。")),
    Parallel("other-congrats", "Other", Map(
      "en" -> "Congratulations on the new role!",
      "fr" -> "Félicitations pour ton nouveau poste !",
      "de" -> "Herzlichen Glückwunsch zur neuen Stelle!",
      "es" -> "¡Enhorabuena por el nuevo puesto!",
      "ru" -> "Поздравляю с новой должностью!",
      "ja" -> "新しい役職おめでとうございます。")),
    Parallel("other-delivery", "Other", Map(
      "en" -> "My order still has not arrived.",
      "fr" -> "Ma commande n'est toujours pas arrivée.",
      "de" -> "Meine Bestellung ist immer noch nicht angekommen.",
      "es" -> "Mi pedido todavía no ha llegado.",
      "ru" -> "Мой заказ до сих пор не пришёл.",
      "ja" -> "注文した品がまだ届きません。")))

  /** the parallel set as (message, class) pairs for one language */
  def inLanguage(lang: String): List[(String, String)] =
    parallel.flatMap(p => p.byLang.get(lang).map(_ -> p.label))

  val classes: List[String] = List("Proposal", "Request", "Notification", "Other")
}
