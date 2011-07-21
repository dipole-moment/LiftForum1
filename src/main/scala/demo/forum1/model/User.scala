package demo.forum1.model



import scala.xml._
import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.mapper._
import net.liftweb.http._
import net.liftweb.http.S._
import liftgert.MapperHelper._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._



trait MyMappedField[FieldType, OwnerType <: Mapper[OwnerType]] extends MappedField[FieldType, OwnerType] {
	override def displayName = S.?(fieldOwner.getSingleton.dbName + "." + name)
}

class MyMappedTextarea[T <: Mapper[T]](owner: T, maxLen: Int) extends MappedTextarea[T](owner, maxLen) with MyMappedField[String, T]

class MyMappedString[T <: Mapper[T]](owner: T, maxLen: Int) extends MappedString[T](owner, maxLen) with MyMappedField[String, T]

class MyMappedBoolean[T <: Mapper[T]](owner: T) extends MappedBoolean[T](owner) with MyMappedField[Boolean, T]

class MyMappedLong[T <: Mapper[T]](owner: T) extends MappedLong[T](owner) with MyMappedField[Long, T]

class MyMappedBinary[T <: Mapper[T]](owner: T) extends MappedBinary[T](owner) with MyMappedField[Array[Byte], T]

class MyMappedDateTime[T <: Mapper[T]](owner: T) extends MappedDateTime[T](owner) with MyMappedField[java.util.Date, T]

class MyMappedLongForeignKey[T <: Mapper[T], O <: KeyedMapper[Long, O]](owner: T, _foreignMeta: => KeyedMetaMapper[Long, O]) extends MappedLongForeignKey[T, O](owner, _foreignMeta) with MyMappedField[Long, T]



class User extends MegaProtoUser[User] {

	def getSingleton = User
	
	object user_type extends MyMappedLongForeignKey(this, UserType)

	object personal_essay extends MyMappedTextarea(this, 2048) {
		override def textareaRows  = 10
		override def textareaCols = 50
	} // object personalEssay

	object is_admin extends MyMappedBoolean(this) {
		override def writePermission_? = false
		///override def displayName = "Has Administrator Rights" // TODO: i18n
		override def _toForm: Box[NodeSeq] = super._toForm.map(nodeseq => ("input [disabled]" #> "true")(nodeseq))
	} // object is_admin

	// Let's add a picture to the user just to see how it goes:

	object picture_mime extends MyMappedString(this, 100)

	object picture_update_time extends MyMappedLong(this)

	object picture_data extends MyMappedBinary(this) {
		override def asHtml = {
			if ( picture_mime.is == null || picture_mime.is == "" )
				Text("<no picture>")
			else
				<span>
					picture_mime = {picture_mime.is}<br/>
					picture_update_time = {picture_update_time.is}<br/>
					picture_data = {picture_data.is.size} bytes<br/>
					<img src={"/picture/" + id.is} />
				</span>
		} // override def asHtml

		override def _toForm = {
			Full(
				<p>
					{asHtml}
				</p>
			::	SHtml.fileUpload(
					fileHolder =>
						fileHolder match {
						// An empty upload gets reported with a null mime type,
						// so we need to handle this special case
						case FileParamHolder(_, null, _, _) =>
							// No upload - done.
							println("fileUpload callback: No upload - done.")
						case FileParamHolder(_, mime, _, data) if mime.startsWith("image/") =>
							// Correct upload file type - upload it:
							println("fileUpload callback: Correct upload file type - upload it")
							picture_data(data)
							picture_mime(mime)
							picture_update_time(Helpers.millis)
						case _ =>
							// Wrong upload file type - report error:
							S.error(S.?("user.message.upload.invalid"))
							println("fileUpload callback: Wrong upload file type - report error")
					}
				)
			::	Nil
			)
		} // override def _toForm
	} // object picture_data

	object password_needs_changing extends MyMappedBoolean(this)

} // class User



object User extends User with MetaMegaProtoUser[User] {

	override def dbTableName = "users"

	def redirectBack() = S.redirectTo(S.referer openOr homePage)

	override def screenWrap = Full(
		<lift:surround with="default" at="content">
			<lift:bind />
		</lift:surround>
	)

	override def fieldOrder = List(
		id,
		firstName,
		lastName,
		email,
		locale,
		timezone,
		password,
		personal_essay,
		is_admin,
		picture_data
	)

	// comment this line out to require email validations
	override def skipEmailValidation = true

	override def signupFields: List[FieldPointerType] = List(
		firstName,
		lastName,
		email,
		locale,
		timezone,
		password,
		personal_essay,
		is_admin,
		picture_data
	)

	override def editFields: List[FieldPointerType] = List(
		firstName,
		lastName,
		email,
		locale,
		timezone,
		personal_essay,
		is_admin,
		picture_data
	)

	override def signupXhtml (user: User) = {
		//
		// Override signupXhtml to use a multipart form post (needed for the picture_data upload):
		//
		<form
			method="post"
			action={S.uri}
			enctype="multipart/form-data"
		>
			<table>
				{localForm(user, false, signupFields)}
				<tr>
					<td>
						&nbsp;
					</td>
					<td>
						<user:submit/>
					</td>
				</tr>
		</table></form>
	} // override def signupXhtml

	override def editXhtml (user: User) = {
		//
		// Override editXhtml to use a multipart form post (needed for the picture_data upload):
		//
		<form
			method="post"
			action={S.uri}
			enctype="multipart/form-data"
		>
			<table>
				{localForm(user, true, editFields)}
				<tr>
					<td>
						&nbsp;
					</td>
					<td>
						<user:submit/>
					</td>
				</tr>
			</table>
		</form>
	} // override def editXhtml

	object Existing {
		def unapply (in_s_user_id: String): Option[User] =
			User.find(By(User.id, in_s_user_id.toInt))
	}

	object ExistingOrNew {
		def unapply (in_s_user_id: String): Option[User] =
			if ( in_s_user_id == "new" )
				Some(User.create)
			else
				User.find(By(User.id, in_s_user_id.toInt))
	}

	def matcher: LiftRules.DispatchPF = {
		//
		// A URL matcher for LiftRules.dispatch.append(User.matcher)
		//
		case req @ Req("picture" :: Existing(user) :: Nil, _, GetRequest) =>
			///println("PictureLogic: Found Existing user")
			() => servePicture(user, req)
	}

	def servePicture (user: User, req: Req) : Box[LiftResponse] = {
		if ( ! req.testIfModifiedSince(user.picture_update_time) ) {
			// If not modified, optimized to tell browser to use last good version:
			println("servePicture: Not modified since cached.")
			Full(
				InMemoryResponse(
					new Array[Byte](0),
					List("Last-Modified" -> toInternetDate(user.picture_update_time.is)),
					Nil,
					304 // HttpCode.NotModified
				)
			)
		} else {
			// Serve the Picture:
			println("servePicture: Sending picture.")
			Full(
				InMemoryResponse(
					user.picture_data.is,
					List("Last-Modified" -> toInternetDate(user.picture_update_time.is),
					"Content-Type" -> user.picture_mime.is,
					"Content-Length" -> user.picture_data.is.length.toString),
					Nil /*cookies*/,
					200 // HttpCode.Ok
				)
			)
		}
	}//end servePicture

	// Additional menus...
	
	// The "List" menu:

	def listSuffix = "list"
	lazy val listPath = thePath(listSuffix)

	def listXhtml = {
		<h3><lift:loc>user.label.list</lift:loc></h3>
		<table>
			<tr id="existing">
				<td color="#eee"><a id="delete" ><lift:loc>user.action.delete</lift:loc></a></td>
				<td color="#eee"><a id="edit"   ><lift:loc>user.action.edit</lift:loc></a></td>
				<td><a id="display"><span id="name"/></a></td>
			</tr>
			<tr id="new">
				<td></td>
				<td><a id="add"><lift:loc>user.action.add</lift:loc></a></td>
				<td></td>
			</tr>
		</table>
	}

	def list = {
		(
			"#existing" #> (
				User.findAll.map(user =>
					"#delete  [href]" #> ("delete?userId="  + user.id.is) &
					"#edit    [href]" #> ("editAny?userId=" + user.id.is) &
					"#display [href]" #> ("display?userId=" + user.id.is) &
					"#name" #> (user.niceName)
				)
			) &
			"#new" #> (
					"#add    [href]" #> ("editAny?userId=new")
			)
		) (listXhtml)
	}

	def listMenuLoc: Box[Menu] = Full(Menu(Loc(
		"list",
		listPath,
		S.?("user.action.list"),
		List(
			Template(() => wrapIt(list))
		)
	)))

	// The "Display" menu:

	def displaySuffix = "display"
	lazy val displayPath = thePath(displaySuffix)

	def displayXhtml = {
		<h3><lift:loc>user.label.display</lift:loc></h3>
		<table>
			<div id="rows"/>
		</table>
	}

	def display = {
		val user = S.param("userId") match {
			case Empty =>
				// No userId=... URL parameter: display the current user.
				if ( loggedIn_? ) {
					currentUser.open_!
				} else {
					S.error(S.?("user.message.must_log_in"))
					redirectBack()
				}
			case Full(Existing(o_existingUser)) =>
				// A userId=... URL parameter: display an existing user.
				o_existingUser
			case _ =>
				S.error("PROGRAMMING ERROR in User.mutateUserOnEdit")
				redirectBack()
		}
		
		val html =
			if ( true ) {
				toHtml(user)
			} else {
				signupFields.flatMap(field =>
					field.asHtml
				)
			}

		(
			"#rows" #> html
		) (displayXhtml)
	}

	def displayMenuLoc: Box[Menu] = Full(Menu(Loc(
		"display",
		displayPath,
		S.?("user.action.display"),
		List(
			Template(() => wrapIt(display))
		)
	)))

	// The "Delete" menu:

	def deleteSuffix = "delete"
	lazy val deletePath = thePath(deleteSuffix)

	def deleteXhtml = {
		<h3><lift:loc>user.label.delete</lift:loc></h3>
		<form method="post">
			<table>
				<div id="rows"/>
				<tr>
					<td>&nbsp;</td><td><input id="delete"/></td>
				</tr>
			</table>
		</form>
	}

	def delete = {
		val user = S.param("userId") match {
			case Empty =>
				// No userId=... URL parameter: Don't implicitly delete the current user.
				S.error(S.?("user.message.delete_which"))
				redirectBack()
			case Full(Existing(o_existingUser)) =>
				// A userId=... URL parameter: delete an existing user.
				o_existingUser
			case _ =>
				S.error("PROGRAMMING ERROR in User.mutateUserOnEdit")
				redirectBack()
		}
		
		val html =
			if ( true ) {
				toHtml(user)
			} else {
				signupFields.flatMap(field =>
					field.asHtml
				)
			}
		
		def submitDelete = {
			val s_user = user.niceName
			user.delete_!
			S.notice(S.?("user.done.delete", s_user))
			redirectBack()
		}

		(
			"#rows" #> html &
			"#delete" #> SHtml.submit(S.?("user.confirm.delete"), submitDelete _)
		) (deleteXhtml)
	}

	def deleteMenuLoc: Box[Menu] = Full(Menu(Loc(
		"delete",
		deletePath,
		S.?("user.action.delete"),
		List(
			Hidden,
			Template(() => wrapIt(delete))
		)
	)))

	// The "editAny" menu:

	def editAnySuffix = "editAny"
	lazy val editAnyPath = thePath(editAnySuffix)

	def editAnyXhtml = {
		<h3><lift:loc>user.label.edit</lift:loc></h3>
		<form method="post" enctype="multipart/form-data">
			<table>
				<div id="rows"/>
				<tr>
					<td>&nbsp;</td><td><input id="save"/></td>
				</tr>
			</table>
		</form>
	}

	def editAny = {
		val user = S.param("userId") match {
			case Empty =>
				// No userId=... URL parameter: edit the current user.
				if ( loggedIn_? ) {
					currentUser.open_!
				} else {
					S.error(S.?("user.message.must_log_in"))
					redirectBack()
				}
			case Full(ExistingOrNew(o_existingOrNewUser)) =>
				// A userId=new URL parameter: edit a newly created user.
				o_existingOrNewUser
			case _ =>
				throw new Exception("PROGRAMMING ERROR in User.mutateUserOnEdit")
		}

		def testEdit() {
			user.validate match {
				case Nil =>
					user.save
					S.notice(S.?("user.done.edit", user.niceName))
					S.redirectTo("display?userId=" + user.id.is)

				case xs =>
					S.error(xs)
					editFunc(Full(innerEdit _))
			}
		} // def testEdit

		def innerEdit = (
				"form [action]" #> S.uri &
				"#rows"         #> localForm(user, false, signupFields) &
				"#save"			#> SHtml.submit(S.?("user.confirm.edit"), testEdit _)
			) (editAnyXhtml)

		innerEdit
	} // def editAny

	def editAnyMenuLoc: Box[Menu] = Full(Menu(Loc(
		"editAny",
		editAnyPath,
		S.?("user.action.edit"),
		List(
			Template(() => wrapIt(editFunc.map(_()) openOr editAny))
		)
	)))

	override lazy val sitemap: List[Menu] = List(
		loginMenuLoc,
		logoutMenuLoc,
		createUserMenuLoc,
		lostPasswordMenuLoc,
		resetPasswordMenuLoc,
		editUserMenuLoc,
		changePasswordMenuLoc,
		validateUserMenuLoc,
		// Add my new menus:
		listMenuLoc,
		displayMenuLoc,
		deleteMenuLoc,
		editAnyMenuLoc
	).flatten(a => a)

} // object User



//class UserType extends MyMapper[UserType] {
class UserType extends LongKeyedMapper[UserType] with IdPK {

	def getSingleton = UserType

	object name extends MyMappedString(this, 100)
	object description extends MyMappedString(this, 500)

	object can_grant_to_users extends MyMappedBoolean(this)		// Set to true for groups that are to "contain" single users
	object can_grant_to_groups extends MyMappedBoolean(this)		// Set to true for groups that are to "contain" other groups

	object granted_to_everyone extends MyMappedBoolean(this)		// Set to true for your "All Visitors" group, if any
	object granted_to_loggedin extends MyMappedBoolean(this)		// Set to true for your "Logged-In Visitors" group, if any
	object granted_to_notloggedin extends MyMappedBoolean(this)	// Set to true for your "Not Logged-In Visitors" group, if any

} // class UserType



//object UserType extends UserType with MyMetaMapper[UserType] {
object UserType extends UserType with LongKeyedMetaMapper[UserType] with CRUDify[Long, UserType] {

} // object UserType



class AdminGrantsAdmin extends LongKeyedMapper[AdminGrantsAdmin] with IdPK {

	def getSingleton = AdminGrantsAdmin

	object granted_admin extends MyMappedLongForeignKey(this, User)
	object granting_admin extends MyMappedLongForeignKey(this, User)
} // class AdminGrantsAdmin



object AdminGrantsAdmin extends AdminGrantsAdmin with LongKeyedMetaMapper[AdminGrantsAdmin] with CRUDify[Long, AdminGrantsAdmin] {

} // object AdminGrantsAdmin



class ModeratorGrantsGroupToModerator extends LongKeyedMapper[ModeratorGrantsGroupToModerator] with IdPK {

	def getSingleton = ModeratorGrantsGroupToModerator

	object granted_moderator extends MyMappedLongForeignKey(this, User)
	object on_group extends MyMappedLongForeignKey(this, User)
	object granting_moderator extends MyMappedLongForeignKey(this, User)
} // class ModeratorGrantsGroupToModerator



object ModeratorGrantsGroupToModerator extends ModeratorGrantsGroupToModerator with LongKeyedMetaMapper[ModeratorGrantsGroupToModerator] with CRUDify[Long, ModeratorGrantsGroupToModerator] {

} // object ModeratorGrantsGroupToModerator



class ModeratorGrantsGroupToReader extends LongKeyedMapper[ModeratorGrantsGroupToReader] with IdPK {

	def getSingleton = ModeratorGrantsGroupToReader

	object granted_reader extends MyMappedLongForeignKey(this, User)
	object on_group extends MyMappedLongForeignKey(this, User)
	object granting_moderator extends MyMappedLongForeignKey(this, User)
} // class ModeratorGrantsGroupToReader



object ModeratorGrantsGroupToReader extends ModeratorGrantsGroupToReader with LongKeyedMetaMapper[ModeratorGrantsGroupToReader] with CRUDify[Long, ModeratorGrantsGroupToReader] {

} // object ModeratorGrantsGroupToReader



class AuthorGrantsArticleToGroup extends LongKeyedMapper[AuthorGrantsArticleToGroup] with IdPK {
	/*
		The author has granted the group the right to read the article.

		For the grant to be effective:
		-	The author must have moderation rights for the group, or one of its super-groups.
		-	Or some moderator must have moderation rights for the group, or one of its super-groups,
			and must have granted the read right for the article to the group, or one of its super-groups.

		Inserted when the moderator grants the article to the group.
		Deleted when the moderator revokes the article from the group.
		Deleted when the author updates the article.
	*/

	def getSingleton = AuthorGrantsArticleToGroup

	object granted_group extends MyMappedLongForeignKey(this, User)
	object on_article extends MyMappedLongForeignKey(this, Article)
} // class AuthorGrantsArticleToGroup



object AuthorGrantsArticleToGroup extends AuthorGrantsArticleToGroup with LongKeyedMetaMapper[AuthorGrantsArticleToGroup] with CRUDify[Long, AuthorGrantsArticleToGroup] {

} // object AuthorGrantsArticleToGroup



class ModeratorGrantsArticleToGroup extends LongKeyedMapper[ModeratorGrantsArticleToGroup] with IdPK {
	/*
		The moderator has granted the group the right to read the article.

		For the grant to be effective:
		-	The article's author must have granted the article to the group, or one of its super-groups.
		-	And the moderator must have moderation rights for the group.

		For the moderator to have moderation right for the group:
		-	They have to be an admin.
		-	Or another moderator must have moderation rights for the group, or one of its super-groups,
			and must have granted moderation rights for the group, or one of its super-groups, to this moderator.

		Inserted when the moderator grants the article to the group.
		Deleted when the moderator revokes the article from the group.
		Deleted when the author updates the article.
	*/

	def getSingleton = ModeratorGrantsArticleToGroup

	object granted_group extends MyMappedLongForeignKey(this, User)
	object on_article extends MyMappedLongForeignKey(this, Article)
	object granting_moderator extends MyMappedLongForeignKey(this, User)
} // class ModeratorGrantsArticleToGroup



object ModeratorGrantsArticleToGroup extends ModeratorGrantsArticleToGroup with LongKeyedMetaMapper[ModeratorGrantsArticleToGroup] with CRUDify[Long, ModeratorGrantsArticleToGroup] {

} // object ModeratorGrantsArticleToGroup



class Article extends LongKeyedMapper[Article] with IdPK {
	/*
		Inserted when the author clicks adds an article with
			"Add First Article"
			"Add Article"
			"Add First Sub-Article"
		or	"Add Sub-Article".

		Updates when the author changes the article with
			"Edit"

		Deleted when the author deletes the article with
			"Delete"
	*/

	def getSingleton = Article

	object parent_article extends MyMappedLongForeignKey(this, Article)
	object time_changed extends MyMappedDateTime(this)
	object author extends MyMappedLongForeignKey(this, User)
	object time extends MyMappedDateTime(this)
	object place extends MyMappedString(this, 100)
	object text extends MyMappedTextarea(this, 1000) {
		override def textareaCols = 100
		override def textareaRows = 10
	}

	object picture_mime extends MyMappedString(this, 100)

	object picture_update_time extends MyMappedLong(this)

	object picture_data extends MyMappedBinary(this) {
		override def asHtml =
			if ( picture_mime.is == null || picture_mime.is == "" )
				Text("<no picture>")
			else
				<span>
					picture_mime = {picture_mime.is}<br/>
					picture_update_time = {picture_update_time.is}<br/>
					picture_data = {picture_data.is.size} bytes
					<img src={"/picture/" + id.is} />
				</span>

		override def _toForm = Full(
			asHtml
		::	SHtml.fileUpload(
				fileHolder =>
					fileHolder match {
					// An empty upload gets reported with a null mime type,
					// so we need to handle this special case
					case FileParamHolder(_, null, _, _) =>
						// No upload - done.
						println("fileUpload callback: No upload - done.")
					case FileParamHolder(_, mime, _, data) if mime.startsWith("image/") =>
						// Correct upload file type - upload it:
						println("fileUpload callback: Correct upload file type - upload it")
						picture_data(data)
						picture_mime(mime)
						picture_update_time(Helpers.millis)
					case _ =>
						// Wrong upload file type - report error:
						S.error(S.?("user.message.upload.invalid"))
						println("fileUpload callback: Wrong upload file type - report error")
				}
			)
		::	Nil
		)
	} // object picture_data

} // class Article



object Article extends Article with LongKeyedMetaMapper[Article] with CRUDify[Long, Article] {

} // object Article



class ModeratorDoneWithArticle extends LongKeyedMapper[ModeratorDoneWithArticle] with IdPK {
	/*
		The moderator is done with granting this article to groups.
		So even if the moderator could grant the article to another group that
		the author wants, he doesn't want the website to remind him any more.

		Inserted when the moderator checks
			"Done granting this article to groups"

		Deleted when the author updates the article.
	*/

	def getSingleton = ModeratorDoneWithArticle

	object moderator extends MyMappedLongForeignKey(this, User)
	object article extends MyMappedLongForeignKey(this, Article)
} // class ModeratorDoneWithArticle



object ModeratorDoneWithArticle extends ModeratorDoneWithArticle with LongKeyedMetaMapper[ModeratorDoneWithArticle] with CRUDify[Long, ModeratorDoneWithArticle] {

} // object ModeratorDoneWithArticle



object InitTablesIfNeeded extends Function0[Unit] {

	def apply (): Unit = {
		println("InitTablesIfNeeded BEGIN")

		val opt_userTypes =
			if ( UserType.find().isEmpty )
				Some(
					new AnyRef {
						// There are no user types yet. Create some:

						val o_userType_user = UserType.create.
							name                  ("Nutzer").
							description           ("Ein angemeldeter Nutzer").
							save_and_return_obj

						val o_userType_group = UserType.create.
							name                  ("Nutzergruppe").
							description           ("Eine Gruppe von Nutzern").
							can_grant_to_users    (true).		// Set to true for groups that are to "contain" single users
							save_and_return_obj

						val o_userType_supergroup = UserType.create.
							name                  ("Obergruppe").
							description           ("Eine Gruppe von Gruppen").
							can_grant_to_groups   (true).		// Set to true for groups that are to "contain" other groups
							save_and_return_obj

						val o_userType_allVisitors = UserType.create.
							name                  ("Alle Besucher").
							description           ("Alle Besucher, angemeldet oder nicht").
							granted_to_everyone   (true).		// Set to true for your "All Visitors" group, if any
							save_and_return_obj

						val o_userType_loggedIn = UserType.create.
							name                  ("Alle angemeldeten Nutzer").
							description           ("Alle angemeldeten Nutzer, egal, welchen Gruppen sie angehören").
							can_grant_to_groups   (true).		// Set to true for groups that are to "contain" other groups
							granted_to_loggedin   (true).		// Set to true for your "Logged-In Visitors" group, if any
							save_and_return_obj

						val o_userType_notLoggedIn = UserType.create.
							name                  ("Unangemeldete Besucher").
							description           ("Alle Besucher, die unangemeldet vorbeischauen").
							granted_to_notloggedin(true).		// Set to true for your "Not Logged-In Visitors" group, if any
							save_and_return_obj
					} // There are no user types yet. Create some
				)
			else
				None

		if ( User.find(By(User.is_admin, true)).isEmpty ) {
			// There is no admin user yet. Create one:

			val ks_firstName = "Gert"
			val ks_lastName  = "Rieger"
			val ks_email     = "lift-tutorial-gert@gert-rieger-edv.de"
			val ks_password  = "xgert123"

			if ( false ) {
				println("User.find BEGIN")

				val ao_by = Array(
					(User.firstName, ks_firstName),
					(User.lastName,  ks_lastName),
					(User.email,     ks_email)
					///(User.password,  ks_password)
				)

				for (i1 <- 0 until ao_by.size) {
					val (o_field, s_value) = ao_by(i1)

					println("User.find(By(User." + o_field.name + ", \"" + s_value + "\"))")

					User.find(
						By(o_field, s_value)
					)
				}

				for (i1 <- 0 until ao_by.size; i2 <- i1 + 1 until ao_by.size) {
					val (o_field1, s_value1) = ao_by(i1)
					val (o_field2, s_value2) = ao_by(i2)

					println("User.find(By(User." + o_field1.name + ", \"" + s_value1 + "\"), By(User." + o_field2.name + ", \"" + s_value2 + "\"))")

					User.find(
						By(o_field1, s_value1),
						By(o_field2, s_value2)
					)
				}

				for (i1 <- 0 until ao_by.size; i2 <- i1 + 1 until ao_by.size; i3 <- i2 + 1 until ao_by.size) {
					val (o_field1, s_value1) = ao_by(i1)
					val (o_field2, s_value2) = ao_by(i2)
					val (o_field3, s_value3) = ao_by(i3)

					println("User.find(By(User." + o_field1.name + ", \"" + s_value1 + "\"), By(User." + o_field2.name + ", \"" + s_value2 + "\"), By(User." + o_field3.name + ", \"" + s_value3 + "\"))")

					User.find(
						By(o_field1, s_value1),
						By(o_field2, s_value2),
						By(o_field3, s_value3)
					)
				}

				println("User.find END")
			}

			val lo_one_existing_user = User.find(
				By(User.firstName, ks_firstName),
				By(User.lastName,  ks_lastName),
				By(User.email,     ks_email)
				///By(User.password,  ks_password) // Don't look for the passwort, to avoid the error message "java.sql.SQLException: No value specified for parameter 5"
			)

			lo_one_existing_user match {
				// Check if the special user exists:

				case Full(o_existing_user) =>
					// The special user exists. Make it an administrator:
					o_existing_user.is_admin(true)
					o_existing_user.save

				case _ =>
					// The special user doesn't exist. Create it as an administrator:

					val o_userTypes = opt_userTypes.getOrElse(
						throw new Exception("InitTablesIfNeeded: Strange situation: Found no admin user, but found user types. You'll have to manually create a admin user, or delete the user types before you can use this web application.")
					)
					
					User.create.
						user_type              (o_userTypes.o_userType_user).
						firstName              (ks_firstName).
						lastName               (ks_lastName).
						email                  (ks_email).
						password               (ks_password).
						password_needs_changing(true).
						save
			} // Check if the special user exists
		} // There is no admin user yet. Create one

		println("InitTablesIfNeeded END")
	} // def apply

} // object InitTablesIfNeeded
