#include <config.h>
#include "xmimsim-gui-updater.h"
#include <curl/curl.h>
#include <json-glib/json-glib.h>
#include <stdlib.h>
#include <string.h>
#include <glib.h>

/*
 *
 * This code allows for the checking of available updates by parsing the JSON output obtained
 * through requesting the tags from the GitHub repo of XMI-MSIM
 *
 *
 *
 */






#define XMIMSIM_GITHUB_TAGS_LOCATION "https://api.github.com/repos/tschoonj/xmimsim/git/refs/tags"
#define XMIMSIM_GITHUB_DOWNLOADS_LOCATION "https://api.github.com/repos/tschoonj/xmimsim/downloads"


struct MemoryStruct {
 	char *memory;
	size_t size;
};

static size_t WriteMemoryCallback(void *contents, size_t size, size_t nmemb, void *userp) {
	size_t realsize = size * nmemb;
	struct MemoryStruct *mem = (struct MemoryStruct *)userp;
     
	mem->memory = realloc(mem->memory, mem->size + realsize + 1);
		        
	memcpy(&(mem->memory[mem->size]), contents, realsize);
	mem->size += realsize;
	mem->memory[mem->size] = 0;
			       
	return realsize;
}


static size_t WriteData(void *ptr, size_t size, size_t nmemb, FILE *stream) {
	size_t written;
	written = fwrite(ptr, size, nmemb, stream);
	return written;

}

struct UrlStruct {
	char *filename;
	char *url;
};

static void check_download_url(JsonArray *array, guint index, JsonNode *node, struct UrlStruct *us) {
	JsonObject *object = json_node_get_object(node);
	if (!json_object_has_member(object,"html_url")) {
		return;
	}
	
	const gchar *html_url_string = json_object_get_string_member(object, "html_url");
	char *filename = strrchr(html_url_string,'/')+1;
	if (strcmp(filename, us->filename) == 0) {
		us->url = strdup(html_url_string);
	}
	fprintf(stdout,"html_url: %s\n",html_url_string);

	return;
}

static void check_version_of_tag(JsonArray *array, guint index, JsonNode *node, char **max_version) {
	JsonObject *object = json_node_get_object(node);
	if (!json_object_has_member(object,"ref")) {
		return;
	}
	
	const gchar *ref_string = json_object_get_string_member(object, "ref");

	//fprintf(stdout,"ref: %s\n",ref_string);
	//discard old tag...
	if (strncmp(ref_string,"refs/tags/XMI-MSIM-",strlen("refs/tags/XMI-MSIM-")) != 0)
		return;

	char *tag_version_str = strrchr(ref_string,'-')+1;
	gdouble tag_version = g_ascii_strtod(tag_version_str,NULL);
	fprintf(stdout,"tag_version: %lf\n", tag_version);
	if (tag_version > g_ascii_strtod(*max_version,NULL)) {
		free(*max_version);
		*max_version = strdup(tag_version_str);
		fprintf(stdout,"New version found: %lf\n",tag_version);
	}
		
	return;
}

static int DownloadProgress(void *clientp, double dltotal, double dlnow, double ultotal, double ulnow) {
	fprintf(stdout,"Progress: %lf/%lf\n",dlnow,dltotal);
	return 0;
}


int check_for_updates(char **max_version_rv) {
	GError *error = NULL;
	JsonParser *parser;
	char curlerrors[CURL_ERROR_SIZE];


	CURL *curl;
	CURLcode res;
	struct MemoryStruct chunk;

	chunk.memory = malloc(1);
	chunk.size = 0;

	curl = curl_easy_init();
	if (!curl) {
		fprintf(stderr,"Could not initialize cURL\n");
		return XMIMSIM_UPDATES_ERROR;
	} 

	curl_easy_setopt(curl, CURLOPT_URL,XMIMSIM_GITHUB_TAGS_LOCATION);
	curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);
	curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);
	curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&chunk);
	curl_easy_setopt(curl, CURLOPT_USERAGENT, "libcurl-agent/1.0");
	curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, curlerrors);
	res = curl_easy_perform(curl);
	if (res != 0) {
		fprintf(stderr,"check_for_updates: %s\n",curlerrors);
		return XMIMSIM_UPDATES_ERROR;
	}
/*	else {
		fprintf(stdout,"%s\n",chunk.memory);
	}*/
	curl_easy_cleanup(curl);




	parser = json_parser_new();
	if (json_parser_load_from_data(parser, chunk.memory, -1,&error) ==  FALSE) {
		if (error) {
			fprintf(stderr,"check_for_updates: %s\n",error->message);
			return XMIMSIM_UPDATES_ERROR;
		}
	}
	JsonNode *rootNode = json_parser_get_root(parser);
	if(json_node_get_node_type(rootNode) != JSON_NODE_ARRAY) {
		fprintf(stderr,"check_for_updates: rootNode is not an Array\n");
		return XMIMSIM_UPDATES_ERROR;
	}
	JsonArray *rootArray = json_node_get_array(rootNode);
	char *max_version = g_strdup(PACKAGE_VERSION);
	char *current_version = g_strdup(max_version);
	json_array_foreach_element(rootArray, (JsonArrayForeach) check_version_of_tag, &max_version);

	int rv;
	if (g_ascii_strtod(max_version, NULL) > g_ascii_strtod(current_version, NULL))
		rv = XMIMSIM_UPDATES_AVAILABLE;
	else
		rv = XMIMSIM_UPDATES_NONE;

	*max_version_rv = strdup(g_strstrip(max_version));	

	g_object_unref(parser);

	return rv;
}



int download_updates(GtkWidget *window, char *max_version) {

	//should only be called when there is actually a new version available...
	//so call check_for_updates before
	
	GError *error = NULL;
	JsonParser *parser;
	char curlerrors[CURL_ERROR_SIZE];


	CURL *curl;
	CURLcode res;
	struct MemoryStruct chunk;
	FILE *fp;

	chunk.memory = malloc(1);
	chunk.size = 0;

	curl = curl_easy_init();
	if (!curl) {
		fprintf(stderr,"Could not initialize cURL\n");
		return 0;
	} 

	//construct filename -> platform dependent!!!
	gchar *filename;


/*
#if defined(MAC_INTEGRATION)
	//Mac OS X
	filename = g_strdup_printf("XMI-MSIM-%s.dmg",max_version);
	
#elif defined(G_OS_WIN32)*/
	//Win 32
	filename = g_strdup_printf("XMI-MSIM-%s-win32.exe",max_version);
/*	
#else
	//Linux??
	//filename = g_strdup_printf("xmimsim-%s.tar.gz",max_version);
	
//#endif
*/
	fprintf(stdout,"filename: %s\n",filename);
	//check if file exists!	
	


	res = curl_easy_setopt(curl, CURLOPT_URL,XMIMSIM_GITHUB_DOWNLOADS_LOCATION);
	if (res != 0) {
		fprintf(stderr,"download_updates: %s\n",curlerrors);
		return 0;
	}
	res = curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);
	if (res != 0) {
		fprintf(stderr,"download_updates: %s\n",curlerrors);
		return 0;
	}
	res = curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);
	if (res != 0) {
		fprintf(stderr,"download_updates: %s\n",curlerrors);
		return 0;
	}
	res = curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&chunk);
	if (res != 0) {
		fprintf(stderr,"download_updates: %s\n",curlerrors);
		return 0;
	}
	res = curl_easy_setopt(curl, CURLOPT_USERAGENT, "libcurl-agent/1.0");
	if (res != 0) {
		fprintf(stderr,"download_updates: %s\n",curlerrors);
		return 0;
	}
	res = curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, curlerrors);
	if (res != 0) {
		fprintf(stderr,"download_updates: %s\n",curlerrors);
		return 0;
	}

	res = curl_easy_perform(curl);
	if (res != 0) {
		fprintf(stderr,"download_updates: %s\n",curlerrors);
		return 0;
	}
	else {
		fprintf(stdout,"x%sx\n",chunk.memory);
	}
	curl_easy_cleanup(curl);

	parser = json_parser_new();
	if (json_parser_load_from_data(parser, chunk.memory, -1,&error) ==  FALSE) {
		if (error) {
			fprintf(stderr,"download_updates: %s\n",error->message);
			return 0;
		}
	}
	JsonNode *rootNode = json_parser_get_root(parser);
	if(json_node_get_node_type(rootNode) != JSON_NODE_ARRAY) {
		fprintf(stderr,"check_for_updates: rootNode is not an Array\n");
		return 0;
	}
	JsonArray *rootArray = json_node_get_array(rootNode);
	struct UrlStruct *us = (struct UrlStruct *) malloc(sizeof(struct UrlStruct));
	us->filename = filename;
	us->url = NULL;

	json_array_foreach_element(rootArray, (JsonArrayForeach) check_download_url, us);
	if (us->url == NULL)
		return 0;

	//spawn dialog
	//write your own code for this


	//construct download location
	gchar *download_location = g_strdup_printf("%s/%s",g_get_user_special_dir(G_USER_DIRECTORY_DOWNLOAD),filename);
	fprintf(stdout,"download_location: %s\n",download_location);

	fp = fopen(download_location, "wb");
	if (fp == NULL) {
		fprintf(stderr,"download_updates: Could not open %s for writing\n",download_location);
		return 0;
	}
	curl = curl_easy_init();
	if (!curl) {
		fprintf(stderr,"Could not initialize cURL\n");
		return 0;
	} 
	curl_easy_setopt(curl, CURLOPT_URL,us->url);
	curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);
	curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteData);
	curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
	curl_easy_setopt(curl, CURLOPT_USERAGENT, "libcurl-agent/1.0");
	curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, curlerrors);
	curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 0L);
	curl_easy_setopt(curl, CURLOPT_PROGRESSFUNCTION, DownloadProgress);

	res = curl_easy_perform(curl);
	if (res != 0) {
		fprintf(stderr,"download_updates: %s\n",curlerrors);
		return 0;
	}
	fclose(fp);
	curl_easy_cleanup(curl);

	return 1;
}


