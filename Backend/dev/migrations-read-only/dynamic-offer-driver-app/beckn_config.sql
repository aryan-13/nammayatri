CREATE TABLE atlas_driver_offer_bpp.beckn_config ();

ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN collected_by text NOT NULL default 'BPP';
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN domain text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN gateway_url text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN payment_params_json text ;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN registry_url text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN settlement_type text ;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN static_terms_url text ;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN subscriber_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN subscriber_url text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN unique_key_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN vehicle_category text NOT NULL default 'CAB';
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.beckn_config ADD PRIMARY KEY ( id);