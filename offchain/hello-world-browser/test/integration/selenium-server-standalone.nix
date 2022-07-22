{ lib
, stdenv
, fetchurl
, makeWrapper
, jre
, chromeSupport ? true
, chromedriver
, htmlunit-driver
, ...
}:
let
  minorVersion = "2.53";
  patchVersion = "1";
in
stdenv.mkDerivation rec {
  pname = "selenium-server-standalone";
  version = "${minorVersion}.${patchVersion}";

  src = fetchurl {
    url = "http://selenium-release.storage.googleapis.com/${minorVersion}/selenium-server-standalone-${version}.jar";
    sha256 = "sha256-HM5tOlylsuMr4YylEH1PIb3aqaGHAOOxF3aPEwQLfPg=";
  };

  dontUnpack = true;

  nativeBuildInputs = [ makeWrapper ];
  buildInputs = [ jre ];

  installPhase = ''
    mkdir -p $out/share/lib/${pname}-${version}
    cp $src $out/share/lib/${pname}-${version}/${pname}-${version}.jar
    makeWrapper ${jre}/bin/java $out/bin/selenium-server \
      --add-flags "-cp $out/share/lib/${pname}-${version}/${pname}-${version}.jar:${htmlunit-driver}/share/lib/${htmlunit-driver.name}/${htmlunit-driver.name}.jar" \
      ${lib.optionalString chromeSupport "--add-flags -Dwebdriver.chrome.driver=${chromedriver}/bin/chromedriver"} \
      --add-flags "org.openqa.grid.selenium.GridLauncherV3"
  '';
}
