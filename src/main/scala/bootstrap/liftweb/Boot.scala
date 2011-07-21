package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor, CRUDify}
import _root_.java.sql.{Connection, DriverManager}
import _root_.demo.forum1.model._


/**
* A class that's instantiated early and run.  It allows the application
* to modify lift's environment
*/
class Boot {

	def boot {
		if ( ! DB.jndiJdbcConnAvailable_? ) {
			val vendor = new StandardDBVendor(
				Props.get("db.driver") openOr "org.h2.Driver",
				Props.get("db.url")    openOr "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
				Props.get("db.user"),
				Props.get("db.password")
			)

			LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

			DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
		}
		
		val lo_mapper = List(
			User,
			UserType,
			AdminGrantsAdmin,
			ModeratorGrantsGroupToModerator,
			ModeratorGrantsGroupToReader,
			AuthorGrantsArticleToGroup,
			ModeratorGrantsArticleToGroup,
			Article,
			ModeratorDoneWithArticle
		)

		// where to search snippet
		LiftRules.addToPackages("demo.forum1")
		Schemifier.schemify(
			true,
			Schemifier.infoF _,
			lo_mapper: _*
		)

		val TemplateHome = Template(() => <lift:embed what="index-embed"/>)
		val lo_mapperMenus =
			lo_mapper.flatMap(
				o_mapper => o_mapper match {
					case o_crudMapper: CRUDify[_, _] =>
						// This mapper has CRUD, so it has menus.
						// Create a new menu that contains the mapper's menus as submenus.
						val s_crudMapper = o_crudMapper.dbName + ".mapper"
						Full(
							Menu(
								Loc(
									s_crudMapper,		// Internal menu name
									List(s_crudMapper),	// Document name. (Just for the URL. We don't actually provide a document file.)
									s_crudMapper,		// Display menu name (as a i18n key).
									TemplateHome		// The same "home" pasge for all mapper menus.
								),
								o_crudMapper.menus: _*	// Submenus.
							)
						)
					case _ =>
						Empty
				}
			)
		
		val lo_menu = List(
			Menu("Home") / "index", // Simple menu form
			Menu("User") / "index_user" >> TemplateHome >> User.AddUserMenusUnder, // With submenus
			// Menu with special Link
			Menu(Loc("Static", Link(List("static"), true, "/static/index"), "Static Content"))
		) :::
			lo_mapperMenus
		
		// Build SiteMap
		def sitemap() = SiteMap(
			lo_menu: _*
		)

		LiftRules.setSiteMapFunc(
			() => User.sitemapMutator(sitemap())
		)

		/*
		* Show the spinny image when an Ajax call starts
		*/
		LiftRules.ajaxStart = Full(
			() => LiftRules.jsArtifacts.show("ajax-loader").cmd
		)

		/*
		* Make the spinny image go away when it ends
		*/
		LiftRules.ajaxEnd = Full(
			() => LiftRules.jsArtifacts.hide("ajax-loader").cmd
		)

		LiftRules.early.append(makeUtf8)

		LiftRules.loggedInTest = Full(() => User.loggedIn_?)

		S.addAround(DB.buildLoanWrapper)

		LiftRules.dispatch.append(User.matcher)
		
		LiftRules.resourceNames = List(
			"user"
		)
		
		InitTablesIfNeeded()
	} // def boot

	/**
	* Force the request to be UTF-8
	*/
	private def makeUtf8(req: HTTPRequest) {
		req.setCharacterEncoding("UTF-8")
	}
} // class Boot
